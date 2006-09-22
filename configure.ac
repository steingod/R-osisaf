dnl Process this file with autoconf to produce a configure script

AC_PREREQ(2.59)

AC_INIT([osisaf R-package],[0.1],[o.godoy@met.no])

AC_LANG(C)

dnl ########################################################### 
dnl Check status of HDF5
dnl 
AC_ARG_WITH([hdf5],
    AC_HELP_STRING([--with-hdf5=DIR],
    [the location of HDF5 files and library either as DIR or INC,LIB]),,[with_hdf5=yes])
if test [ -n "$with_hdf5" ] ; then
  case $with_hdf5 in
    yes)
     echo "Using system implementation of HDF5"
     ;;
    *,*)
      hdf5_inc="`echo $with_hdf5 | cut -f1 -d,`"
      hdf5_lib="`echo $with_hdf5 | cut -f2 -d,`"
      CPPFLAGS="$CPPFLAGS -I$hdf5_inc"
      LIBS="$LIBS -L$hdf5_lib"
      ;;
    *)
      hdf5_inc="$with_hdf5/include"
      hdf5_lib="$with_hdf5/lib"
      CPPFLAGS="$CPPFLAGS -I$hdf5_inc"
      LIBS="$LIBS -L$hdf5_lib"
      ;;
  esac
else
  AC_MSG_ERROR([HDF5 library is required, use --with-hdf5=DIR or INC,LIB])
fi
saved_CPPFLAGS="$CPPFLAGS"
saved_LIBS="$LIBS"
AC_CHECK_LIB([hdf5],[H5Fopen],
,[AC_MSG_ERROR([HDF5 library is required, use --with-hdf5=DIR or INC,LIB])])

    
dnl ########################################################### 
dnl Check status of OSIHDF5
dnl 
AC_ARG_WITH([osihdf5],
    AC_HELP_STRING([--with-osihdf5=DIR],
    [the location of OSIHDF5 files and library either as DIR or INC,LIB]))
if test [ -n "$with_osihdf5" ] ; then
  case $with_osihdf5 in
    *,*)
      osihdf5_inc="`echo $with_osihdf5 | cut -f1 -d,`"
      osihdf5_lib="`echo $with_osihdf5 | cut -f2 -d,`"
      ;;
    *)
      osihdf5_inc="$with_osihdf5/include"
      osihdf5_lib="$with_osihdf5/lib"
      ;;
  esac
  CPPFLAGS="$CPPFLAGS -I$osihdf5_inc"
  LIBS="$LIBS -L$osihdf5_lib"
else
  AC_MSG_ERROR([OSIHDF5 library is required, use --with-osihdf5=DIR or INC,LIB])
fi
saved_CPPFLAGS="$CPPFLAGS"
saved_LIBS="$LIBS"
AC_CHECK_LIB([osihdf5],[init_osihdf],
,[AC_MSG_ERROR([OSIHDF5 library is required, use --with-osihdf5=DIR or INC,LIB])])

dnl ########################################################### 
dnl Check status of the SATIMG library
dnl
#AC_ARG_WITH([satimg],
#    AC_HELP_STRING([--with-satimg=DIR],
#    [the location of SATIMG files and library either as DIR or
#    INC,LIB]),,[with_satimg=yes])
#if test [ -n "$with_satimg" ] ; then
#  case $with_satimg in
#    *,*)
#      satimg_inc="`echo $with_satimg | cut -f1 -d,`"
#      satimg_lib="`echo $with_satimg | cut -f2 -d,`"
#      ;;
#    *)
#      satimg_inc="$with_satimg/include"
#      satimg_lib="$with_satimg/lib"
#      ;;
#  esac
#  CPPFLAGS="$CPPFLAGS -I$satimg_inc"
#  LIBS="$LIBS -L$satimg_lib"
#else
#  AC_MSG_ERROR([SATIMG library is required, use --with-satimg=DIR or INC,LIB])
#fi
#saved_CPPFLAGS="$CPPFLAGS"
#saved_LIBS="$LIBS"
#AC_CHECK_LIB([satimg],[ivec],
#,[AC_MSG_ERROR([SATIMG library is required, use --with-satimg=DIR or INC,LIB])])

dnl ########################################################### 
dnl Some R specific tasks...
dnl
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`

AC_CONFIG_FILES(src/Makevars)
AC_SUBST(CFLAGS)
AC_SUBST(LIBS)
AC_OUTPUT
