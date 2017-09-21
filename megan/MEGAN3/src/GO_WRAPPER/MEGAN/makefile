SHELL=/bin/sh
FC= gfortran
FFLAGS= -O0 -ffixed-line-length-none -Wall -fcheck=bounds -fPIC -g -fbacktrace
LIBRARY_STATIC= libmegan.a
INCLUDE = -I$(CURDIR)/INCLDIR

OBJS = \
     checkmem.o \
     findlai.o \
     soilnox_fx_noioapi.o \
     soilnox.o \
     growseason_noioapi.o \
	 julday.o \
	 gregorian.o \
	 incrementDateTime.o \
	 megsea_module.o \
	 megcan_module.o \
	 megvea_module.o \
	 mgn2mech_module.o

MODULES= *mod.f

#-----------------------------------------------------------------------
# line to allow file.mod files to be recognised by GNU make
%.o : %.mod
#-----------------------------------------------------------------------

.SUFFIXES : .f .o 

.f.o:
	$(FC) -c $(FFLAGS) $(INCLUDE) $<
.F.o:
	$(FC) -c $(FFLAGS) $(INCLUDE) $<

#-----------------------------------------------------------------------

$(LIBRARY_STATIC): $(OBJS)
	ar rcs $(LIBRARY_STATIC) $(OBJS)
	
#-----------------------------------------------------------------------

clean:
	rm -f *.o *.mod *.core *.a
	
#-----------------------------------------------------------------------

all: clean $(LIBRARY_STATIC)

#-----------------------------------------------------------------------


