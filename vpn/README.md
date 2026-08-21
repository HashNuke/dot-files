# TinyVPN

TinyVPN runs an HTTP proxy through a WireGuard tunnel in an Apple container.
One shared Alpine image supports all VPN configurations.
Each running configuration has a separate container and VM address.

## Requirements

- Install Apple `container`.
- Install `jq`.
- Put the WireGuard configuration files in one private directory.
- Set `CONTAINER_VPN_CONF_PATH` to that directory.

For example, the directory can contain these files:

```text
spain.conf
singapore.conf
```

The configuration files contain private keys.
Do not add them to this repository.

## Build the shared image

Run this command from any directory:

```sh
run-vpn build
```

The `start` command also builds `tinyvpn:local` if the image is not present.
It does not build a separate image for each VPN configuration.

## Start VPN proxies

Start the Spain proxy:

```sh
run-vpn start spain
```

The command prints output in this form:

```sh
VPN for spain is running.
export HTTP_PROXY='http://192.168.64.8:8888' HTTPS_PROXY='http://192.168.64.8:8888' http_proxy='http://192.168.64.8:8888' https_proxy='http://192.168.64.8:8888'
```

Copy and run the `export` line in the current shell.
Only commands that use these proxy variables use the VPN.

Start another proxy without stopping the first proxy:

```sh
run-vpn start singapore
```

This command starts `tinyvpn-singapore` from `singapore.conf`.
The first proxy continues to run as `tinyvpn-spain`.

## Stop VPN proxies

Stop one proxy:

```sh
run-vpn stop spain
```

Stop all running TinyVPN containers:

```sh
run-vpn stop --all
```

Remove the proxy variables when they are not necessary:

```sh
unset HTTP_PROXY HTTPS_PROXY http_proxy https_proxy
```

## List VPN proxies

```sh
run-vpn list
```

This command lists only running `tinyvpn-*` containers.

## List configurations

List the configuration names that can be used with `start`:

```sh
run-vpn configs
```

## Help

```sh
run-vpn help
```

The image accepts any compatible WireGuard configuration.
The configuration must route IPv4 traffic with `AllowedIPs = 0.0.0.0/0`.
TinyVPN blocks IPv6 egress in the container.
