
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
export -TU IKARUS_LIBRARY_PATH ikarus_library_path
export -TU GI_TYPELIB_PATH gi_typelib_path
export -TU YPSILON_SITELIB ypsilon_sitelib

. ~/etc/sysinfo-common.sh

if command -v python > /dev/null; then
  pyversion=`python -c 'import sys; print sys.version[0:3]'`
  python_path=(~/$sysdir/lib/python$pyversion/site-packages $python_path)
fi

fpath=(~/.zsh/functions $fpath)
path=(~/bin ~/$sysdir/bin $path /usr/local/games)
ld_library_path=(~/$sysdir/lib $ld_library_path)
#ltdl_library_path=(~/$sysdir/lib $ltdl_library_path)
guile_load_path=(~/$sysdir/share/guile/site ~/$sysdir/share/guile /usr/local/share/guile/site $guile_load_path)
pkg_config_path=(~/$sysdir/lib/pkgconfig ~/$sysdir/share/pkgconfig $pkg_config_path /usr/local/lib/pkgconfig)
ikarus_library_path=(~/src/spe/targets/ikarus)
ypsilon_sitelib=(~/src/spe/targets/ypsilon)
gi_typelib_path=(~/$sysdir/lib/girepository-1.0)

export ACLOCAL_FLAGS="-I /usr/local/share/aclocal -I $HOME/$sysdir/share/aclocal $ACLOCAL_FLAGS"
export SCSH_LIB_DIRS=\"$HOME'/$sysdir/lib/scsh/0.6" '$SCSH_LIB_DIRS
export CHICKEN_REPOSITORY="$HOME/$sysdir/lib/chicken"

# Preferences

export BROWSER="firefox -n"
export PAGER=less
export EDITOR=edit
export CVS_RSH=ssh
export CCACHE_UNIFY=yes
export LANG=C
if [ -f /etc/locale.gen ] && grep -q ^de_AT.UTF-8 /etc/locale.gen; then
  export LC_CTYPE=de_AT.UTF-8
fi
export PYTHONSTARTUP="$HOME/.pythonrc"
export PYTHONDOCS="/usr/share/doc/python2.2-doc/html"
export PLTCOLLECTS="$PLTCOLLECTS:$HOME/src/spe/targets/mzscheme"
export XDG_DATA_DIRS="$HOME/$sysdir/share:/usr/share"

# Personal info

export EMAIL="a.rottmann@gmx.at"
export DEBEMAIL="Andreas Rottmann <rotty@debian.org>"
export REPORTBUGEMAIL="Andreas Rottmann <a.rottmann@gmx.at>"
export DARCS_EMAIL="Andreas Rottmann <rotty@debian.org>"
