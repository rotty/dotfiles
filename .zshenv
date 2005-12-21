
#
# Paths
#

typeset -U path
export -TU TEXINPUTS texinputs
export -TU MFINPUTS mfinputs
export -TU LD_LIBRARY_PATH ld_library_path
export -TU LTDL_LIBRARY_PATH ltdl_library_path
export -TU MOZILLA_PLUGIN_PATH mozilla_plugin_path
export -TU GUILE_LOAD_PATH guile_load_path
export -TU PKG_CONFIG_PATH pkg_config_path
export -TU PYTHONPATH python_path
export -TU SERVEEZ_LOAD_PATH serveez_load_path

if command -v python > /dev/null; then
  pyversion=`python -c 'import sys; print sys.version[0:3]'`
  python_path=(~/.system/lib/python$pyversion/site-packages $python_path)
fi

fpath=(~/.zsh/functions $fpath)
path=(~/bin ~/.system/bin $path /usr/local/games)
ld_library_path=(~/.system/lib $ld_library_path)
#ltdl_library_path=(~/.system/lib $ltdl_library_path)
guile_load_path=(~/.system/share/guile/site ~/.system/share/guile /usr/local/share/guile/site $guile_load_path)
pkg_config_path=(~/.system/lib/pkgconfig $pkg_config_path /usr/local/lib/pkgconfig)

export ACLOCAL_FLAGS="-I /usr/local/share/aclocal $ACLOCAL_FLAGS"
export SCSH_LIB_DIRS=\"$HOME'/.system/lib/scsh/0.6" '$SCSH_LIB_DIRS
export CHICKEN_REPOSITORY="$HOME/.system/lib/chicken"

# Preferences

export BROWSER="firefox -n"
export PAGER=less
export EDITOR=zile
export CVS_RSH=ssh
export CCACHE_UNIFY=yes
export LANG=C
if [ -f /etc/locale.gen ] && grep -q de_AT.UTF-8 /etc/locale.gen; then
  export LC_CTYPE=de_AT.UTF-8
fi
export PYTHONSTARTUP="$HOME/.pythonrc"
export PYTHONDOCS="/usr/share/doc/python2.2-doc/html"

# Personal info

export EMAIL="a.rottmann@gmx.at"
export DEBEMAIL="Andreas Rottmann <rotty@debian.org>"
export REPORTBUGEMAIL="Andreas Rottmann <a.rottmann@gmx.at>"
