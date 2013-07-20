Vagrant.configure('2') do |config|
  config.vm.box_url = 'http://zxaos.github.io/vagrantbox-raringServer64/raringServer64.box'
  config.vm.box     = 'raring-server'

  config.ssh.forward_agent = true

  config.vm.provision :shell, :inline => $bootstrap

  config.vm.provider(:virtualbox) do |vb|
    vb.customize ["modifyvm", :id, "--memory", "4096"]
    vb.customize ["modifyvm", :id, "--cpus", "2"]
  end

  config.vm.synced_folder ".", "/home/vagrant/statgrab"
end

$bootstrap = <<-SCRIPT
updated=~/.apt-updated

if [ ! -f $updated ]; then apt-get update && touch $updated; fi

apt-get install -y \
 build-essential \
 man \
 git-core \
 zlib1g-dev \
 libstatgrab-dev \
 ghc \
 cabal-install

su - vagrant -c "
echo 'export PATH=~/.cabal/bin:$PATH' > ~/.bashrc

cabal update && {
    cabal install cabal-install
    cabal install cabal-dev cabal-meta alex happy c2hsc
}
"
SCRIPT
