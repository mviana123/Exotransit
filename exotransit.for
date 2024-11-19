      program exotransit
        implicit none

        real, parameter :: pi = 3.14159265358979323846
        real, parameter :: blumratio = 0.0029
        real, parameter :: sradius = 0.411
        real, parameter :: operiod = 365.25
        real, parameter :: nlevel = 0.05
        real, parameter :: dfactor = 1070.0
        real, parameter :: minexoradius = 0.01
        real :: lumratio, exoradius, angle, pincrement
        real :: brightness, nbright, obright, noise
        integer :: i, nradii, pcount, j, count,k, numoperiods
        logical :: detectable
        real :: minradius = 1.0E30

        open(unit=10, file='brightness_data.csv', 
     &       status='replace')
        write(10, '("Radius, Time (Orbital Periods), Brightness")')

        call System_Clock(count)
        call srand(count)

        lumratio = blumratio / (dfactor ** 2)
        pincrement = 2.0 * pi / operiod

        nradii = 100
        numoperiods = 3
        pcount = 365

        do i = 1, nradii
          exoradius = max(minexoradius, minexoradius + 
     &                    0.01 * real(i -1))
          detectable = .false.


          do k = 1, numoperiods
            do j = 1, pcount
              angle = pincrement * real(j - 1)

              if (abs(sin(angle)) .le. sradius) then
                nbright = lumratio * (1.0 - (pi * exoradius**2)
     &                              / (pi * sradius**2))
                else
                  nbright = lumratio
                endif

              noise = (rand()- 0.5) * 0.1 * nbright
              obright = nbright + noise
              write(10, '(f8.4, ", ", i6, ", ", e12.5)')
     &        exoradius, int(j + (k - 1) * pcount), obright

              if(abs(obright - lumratio) .gt. 
     &           (nlevel * lumratio)) then
                detectable = .true.
              END if
            END do
          END do

        if(detectable .and. exoradius .lt. minradius) then
            minradius = exoradius
          END if

      END do

      close(unit=10)

      if(minradius .lt. 1.0E30) then
        write(*,*) 'Min detectable radius:', minradius, 'solar radii'
      else
        write(*,*) 'No detectable exoplanet in range.'
      END if

      stop

      END program exotransit
