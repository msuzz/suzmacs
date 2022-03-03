#!/usr/bin/env sh
# 2022, Michael Suzzi

# Thanks to
# https://unix.stackexchange.com/questions/388194/shell-script-to-retrieve-text-from-website
# for help

pde_url='https://metacpan.org/pod/Emacs::PDE'
html=$( curl -# -L "${pde_url}" 2> '/dev/null' )
pde_ver=$( <<< "${html}" grep -Poe '(?<=Module version: )(v.+)' | head -n 1 )
pde_dl="https://cpan.metacpan.org/authors/id/Y/YE/YEWENBIN/Emacs-PDE-${pde_ver}.tar.gz"

printf '%s\n' "Latest PDE version: ${pde_ver}"
printf '%s\n' "Downloading from ${pde_dl}..."

wget -P ./lisp/pde "${pde_dl}"
tar xzf "./lisp/pde/Emacs-PDE-${pde_ver}.tar.gz" -C ./lisp/pde
mv -v ./lisp/pde/Emacs-PDE-${pde_ver}/* ./lisp/pde/
rm -vd ./lisp/pde/Emacs-PDE-${pde_ver}

printf '%s\n' "Emacs::PDE ${pde_ver} successfully installed to ./lisp/pde/"
