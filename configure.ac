AC_INIT([Haskell ttree package], [0.1], [cspollard@gmail.com], [ttree])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([ttree.cabal])

# Build the package if we found X11 stuff
X_INC_DIRS=`root-config --incdir`
X_LIB_DIRS=`root-config --libdir`

AC_SUBST([X_INC_DIRS])
AC_SUBST([X_LIB_DIRS])
AC_CONFIG_FILES([ttree.buildinfo])
AC_OUTPUT
