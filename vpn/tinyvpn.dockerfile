FROM alpine:3.24

RUN apk add --no-cache \
      ca-certificates \
      iproute2 \
      iptables \
      openresolv \
      tinyproxy \
      wireguard-go \
      wireguard-tools \
    && install -d -m 0700 /etc/wireguard

COPY tinyvpn-entrypoint.sh /usr/local/bin/tinyvpn
COPY tinyvpn-ipv4-config.awk /usr/local/libexec/tinyvpn-ipv4-config.awk
COPY tinyvpn-tinyproxy.conf /etc/tinyproxy/tinyproxy.conf

RUN chmod 0755 /usr/local/bin/tinyvpn

EXPOSE 8888

ENTRYPOINT ["/usr/local/bin/tinyvpn"]
