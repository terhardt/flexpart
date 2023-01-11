subroutine writeinitmass(itime, nparts, totalmass)

  use point_mod
  use par_mod
  use com_mod

  implicit none
  real:: totalmass
  integer:: nparts
  integer :: jjjjmmdd,ihmmss,itime,i,j,ks
  real(kind=dp) :: jul
  character :: adate*8,atime*6, outname*34

  integer :: ix,jy,imem
  real :: xp1,yp1
  
  call caldate(bdate,jjjjmmdd,ihmmss)
  write(adate,'(i8.8)') jjjjmmdd
  write(atime,'(i6.6)') ihmmss
  if (DRYBKDEP) then
    outname='initmass' // '_drydep_'//adate//atime//'.txt'
  else 
    outname='initmass' // '_wetdep_'//adate//atime//'.txt'
  endif
  if (itime.eq.0) then
      open(unitmass,file=path(2)(1:length(2))//outname,form='formatted',err=998)
  else
      open(unitmass,file=path(2)(1:length(2))//outname,ACCESS='APPEND',form='formatted',err=998)
  endif

  jul=bdate+real(itime,kind=dp)/86400._dp
  call caldate(jul,jjjjmmdd,ihmmss)
  
  write(adate,'(i8.8)') jjjjmmdd
  write(atime,'(i6.6)') ihmmss
  
  write(unitmass,*)  adate, atime, itime, nparts, totalmass  
  
  close(unitmass)
  return


998   write(*,*) ' #### FLEXPART MODEL ERROR!   THE FILE         #### '
  write(*,*) ' #### '//path(2)(1:length(2))//'header_txt'//' #### '
  write(*,*) ' #### CANNOT BE OPENED. IF A FILE WITH THIS    #### '
  write(*,*) ' #### NAME ALREADY EXISTS, DELETE IT AND START #### '
  write(*,*) ' #### THE PROGRAM AGAIN.                       #### '
  stop

end subroutine writeinitmass
