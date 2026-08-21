#!/bin/sh

set -eu

source_config_path="${TINYVPN_CONFIG:-/etc/wireguard/wg0.conf}"
interface_name="${TINYVPN_INTERFACE:-$(basename "$source_config_path" .conf)}"
runtime_config_dir="/run/wireguard"
runtime_config_path="$runtime_config_dir/$interface_name.conf"
proxy_pid=""
vpn_started="false"
firewall_started="false"
host_gateway=""

usage() {
  cat <<'EOF'
Run a local HTTP proxy through a WireGuard tunnel.

Usage:
  tinyvpn
  tinyvpn --help

Environment:
  TINYVPN_CONFIG     WireGuard configuration path.
                     Default: /etc/wireguard/wg0.conf
  TINYVPN_INTERFACE  Runtime WireGuard interface name.
                     Default: configuration filename without .conf.

The command starts Tinyproxy on port 8888 after the VPN tunnel is ready.
The container requires the NET_ADMIN capability.
EOF
}

remove_firewall() {
  if [ "$firewall_started" != "true" ]; then
    return
  fi

  iptables -D OUTPUT ! -o "$interface_name" -m mark ! --mark "$firewall_mark" \
    -m addrtype ! --dst-type LOCAL -j REJECT 2>/dev/null || true
  ip6tables -D OUTPUT ! -o "$interface_name" -m mark ! --mark "$firewall_mark" \
    -m addrtype ! --dst-type LOCAL -j REJECT 2>/dev/null || true
  iptables -D OUTPUT -d "$host_gateway" -j ACCEPT 2>/dev/null || true
  iptables -D INPUT -p tcp --dport 8888 ! -s "$host_gateway" -j REJECT \
    2>/dev/null || true
  firewall_started="false"
}

stop_services() {
  trap - EXIT HUP INT TERM

  if [ -n "$proxy_pid" ]; then
    kill "$proxy_pid" 2>/dev/null || true
    wait "$proxy_pid" 2>/dev/null || true
  fi

  if [ "$vpn_started" = "true" ]; then
    wg-quick down "$runtime_config_path" 2>/dev/null || true
    vpn_started="false"
  fi

  remove_firewall
}

case "${1:-}" in
  -h|--help)
    usage
    exit 0
    ;;
  "")
    ;;
  *)
    echo "Error: unsupported argument: $1" >&2
    usage >&2
    exit 64
    ;;
esac

if [ ! -r "$source_config_path" ]; then
  echo "Error: WireGuard configuration is not readable: $source_config_path" >&2
  echo "Mount a WireGuard configuration at that path." >&2
  exit 66
fi

case "$interface_name" in
  ""|*[!a-zA-Z0-9_=+.-]*)
    echo "Error: the WireGuard interface name is not valid." >&2
    exit 64
    ;;
esac

trap stop_services EXIT HUP INT TERM

install -d -m 0700 "$runtime_config_dir"
awk -f /usr/local/libexec/tinyvpn-ipv4-config.awk \
  "$source_config_path" >"$runtime_config_path"
chmod 0600 "$runtime_config_path"

host_gateway="$(ip -4 route show default | awk '{print $3; exit}')"
if [ -z "$host_gateway" ]; then
  echo "Error: the container does not have an IPv4 host gateway." >&2
  exit 69
fi

wg-quick up "$runtime_config_path"
vpn_started="true"

firewall_mark="$(wg show "$interface_name" fwmark)"
if [ -z "$firewall_mark" ] || [ "$firewall_mark" = "off" ]; then
  echo "Error: the WireGuard configuration did not install a default route." >&2
  echo "Use a WireGuard configuration with AllowedIPs = 0.0.0.0/0." >&2
  exit 78
fi

iptables -I OUTPUT ! -o "$interface_name" -m mark ! --mark "$firewall_mark" \
  -m addrtype ! --dst-type LOCAL -j REJECT
ip6tables -I OUTPUT ! -o "$interface_name" -m mark ! --mark "$firewall_mark" \
  -m addrtype ! --dst-type LOCAL -j REJECT
iptables -I OUTPUT 1 -d "$host_gateway" -j ACCEPT
iptables -I INPUT -p tcp --dport 8888 ! -s "$host_gateway" -j REJECT
firewall_started="true"

echo "WireGuard tunnel is ready on $interface_name."
echo "HTTP proxy is listening on port 8888."

tinyproxy -d -c /etc/tinyproxy/tinyproxy.conf &
proxy_pid="$!"
wait "$proxy_pid"
