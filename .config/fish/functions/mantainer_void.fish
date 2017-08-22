
function update_master
  cd ~/void-packages/
  git checkout -f master
  git pull upstream master
  xcheckmypkgs
end
