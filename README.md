dotfiles
========

# git setting

$ git config --global init.templatedir ~/.git_template/

$ chmod +x ~/.git_template/hooks/*


# atom package

## install from list

apm install --packages-file atom_packages.txt

## export list

apm list --installed --bare > atom_packages.txt

