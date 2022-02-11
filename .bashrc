# .bashrc

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

# Give terminal 256 colors (rather than 16 or so)
# Also helps emacs look like emacs -nw (or the terminal)
export TERM=xterm-256color

####### add to path ##########################
#add mendeley (end)
# PATH=$PATH:/home/ape/Downloads/mendeleydesktop-1.17.9-linux-x86_64/bin

############# aliases ##########################

# User specific aliases and functions
alias ema='setsid emacs'
alias emac='emacs -nw'

alias open='gnome-open'
alias okular='setsid okular'


alias sizeDirs='du -hsx ./* | sort -rh | head -n 40'

# # quicker ls
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

#sizing
alias sizeDirs='du -hsx ./* | sort -rh | head -n 40'

#alias mendeleydesktop='setsid mendeleydesktop'

# #ssh
# alias sboole='ssh -X ape@boole'
# alias stgpu='ssh -X ape@192.168.1.136'
# alias sisak='ssh -X ape@192.168.1.148'
# alias stesla='ssh -X ape@WS-Tesla-1'
# alias sboyle='ssh -X ape@boyle'


#activate openMPI (only aware of modules in Fedora)
# module load  mpi/mpich-x86_64 

################ things to remember (never uncomment!) ###############
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
