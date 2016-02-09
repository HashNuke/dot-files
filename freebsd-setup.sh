
sudo pkg install -y bash vim ediors/emacs git tmux gmake automake autoconf openssl libyaml readline libxslt libtool unixodbc

sudo pkg install -y openjdk8


sudo sh -c 'echo "proc  /proc   procfs rw  0 0" >> /etc/fstab'
sudo sh -c 'echo "fdesc /dev/fd fdescfs rw 0 0" >> /etc/fstab'
sudo mount -a

echo "PasswordAuthentication yes" >> /etc/ssh/ssh_config
sudo /etc/rc.d/sshd restart
