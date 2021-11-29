# suzmacs
This is my personal Emacs configuration that I have gradually constructed over
the past couple of years.

Features include:  

  - Everything self-contained in ~/.emacs.d directory  
  - Seperate folder for themes (~/emacs.d/themes)  
  - MELPA enabled (naturally)  
  - Many customisations including maximise-on-startup, column indicators at 80,  
    100 and 120, tab width set to 4, minimal toolbars, line numbers, etc  
  - Recent files list with C-x C-r  
  - Redo+ (Undo with C-/, redo with C-?)  
  - Reload current file with C-x C-v  
  - Treemacs (Tree file browser)  
  - Magit (Git user-interface, C-x g to open, C-c M-g for shortcuts)  
  - Java code completion via JDEE (server JAR included)  
  - Use CPerl mode by default for Perl files  
  - Markdown Preview mode  

# Installation
Just clone this repo to ~/.emacs.d and you should be good to go. Make sure there
is no existing .emacs file in your home directory.

# Specifying theme and font
If you want to change the theme or font, I recommend you do this in Emacs
itself, which will write the changes to ~/.emacs.d/custom.el.

For the theme:

> `M-x customize-themes`

For the font:

> `M-x customize-face`

custom.el has been added to the .gitignore file, so your personal preferences
shall remain unmolested if you ever decide to pull updates from this repo.

