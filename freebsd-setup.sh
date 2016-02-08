
sudo pkg install -y bash vim ediors/emacs git tmux automake autoconf openssl libyaml readline libxslt libtool unixodbc

sudo sh -c 'echo "fdesc /dev/fd fdescfs rw 0 0" >> /etc/fstab'
sudo mount -a

echo "PasswordAuthentication yes" >> /etc/ssh/ssh_config
sudo /etc/rc.d/sshd restart
