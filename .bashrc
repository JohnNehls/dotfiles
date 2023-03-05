#!/usr/bin/bash

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH


##############################################################################
# User specific aliases and functions
##############################################################################
# Paths
export PATH="$HOME/Dropbox/bin/:$PATH"

# Give terminal 256 colors (rather than 16 or so)
# Also helps emacs look like emacs -nw (or the terminal)
export TERM=xterm-256color

# Display hiddne files first with dired
export LC_COLLATE="C"

####### add to path ##########################
#add mendeley (end)
# PATH=$PATH:/home/ape/Downloads/mendeleydesktop-1.17.9-linux-x86_64/bin

############# aliases ##########################
# recursivly replace all spaces in filenames with _

# User specific aliases and functions
alias ema='setsid emacs -fs' # -fs = fullscreen
alias em='emacs -nw'
alias open='gnome-open'
alias okular='setsid okular'

# # quicker ls
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

#sizing
alias sizeDirs='du -hsx ./* | sort -rh | head -n 40'


################ things to remember (never uncomment!) ###############
#activate openMPI (only aware of modules in Fedora)
# module load  mpi/mpich-x86_64

##running mx3d template
#/usr/bin/mpiexec -np 8 ./mx3d ./parameters.input -pg 8 1 1

################################## END USER DEFINED ###########################

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
if [ -d ~/.bashrc.d ]; then
	for rc in ~/.bashrc.d/*; do
		if [ -f "$rc" ]; then
			. "$rc"
		fi
	done
fi

unset rc

# # >>> conda initialize >>>
# # !! Contents within this block are managed by 'conda init' !!
# __conda_setup="$('/usr/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
# if [ $? -eq 0 ]; then
#     eval "$__conda_setup"
# else
#     if [ -f "/usr/etc/profile.d/conda.sh" ]; then
#         . "/usr/etc/profile.d/conda.sh"
#     else
#         export PATH="/usr/bin:$PATH"
#     fi
# fi
# unset __conda_setup
# # <<< conda initialize <<<
