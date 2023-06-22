! Molecular Orbital PACkage (MOPAC)
! Copyright (C) 2021, Virginia Polytechnic Institute and State University
!
! MOPAC is free software: you can redistribute it and/or modify it under
! the terms of the GNU Lesser General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! MOPAC is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.

  SUBROUTINE graphics(xx, yy, Action ) 
!
! Subroutine "graphics" looks after all graphical operations
!
! On input xx = x-coordinate of the point from the non-graphics side of the program 
!          yy = y-coordinate of the point from the non-graphics side of the program
!
! Action: An integer with value 1, 2, 3, 6, 99, 100, idicating which graphics task is to be done.
!
#ifdef IFORT
  use ifqwin
#endif
  use common_common, only : line, xscale, yscale, xoffset, yoffset, top_left_x, top_left_y
  implicit none
  interface
  subroutine Details_of_cursor_point(unit, mouseevent, keystate, MouseXpos,MouseYpos)
      INTEGER unit
      INTEGER mouseevent
      INTEGER keystate
      INTEGER MouseXpos
      INTEGER MouseYpos
    end subroutine 
  end interface
  integer, intent (in) :: Action
  real, intent (in) :: xx, yy
  !
  ! Local
  !
 ! real :: xoffset,          &  ! Define the relative position on the monitor
 !         yoffset,          &  ! Define the relative position on the monitor
 !         xscale  =  450.0, &  ! Convert from non-graphics coordinates to graphics coordinates
 !         yscale  =  450.0, &  ! Convert from non-graphics coordinates to graphics coordinates
  real ::        x, y
  integer(4) ::   i, j, k, l, col(3), all_colors(50)
  INTEGER(2) :: color, pick_color
!kms  TYPE (windowconfig) :: myscreen
!kms  TYPE (qwinfo)      :: FrameSize
!kms  TYPE (xycoord) :: xy
  character :: type_font*30, num1*1, num2*1
  logical :: status, l_font = .false.
!
! All colors used for lines, expressed as three fractions per color, blue,green,red
!
  real, save, dimension (3,50) :: colors 

  colors = reshape((/ &
  & 0.0, 0.0, 1.0, 0.0, 0.1, 0.9, 0.0, 0.2, 0.8, 0.0, 0.3, 0.7, 0.0, 0.4, 0.6, &
  & 0.0, 0.5, 0.5, 0.0, 0.6, 0.4, 0.0, 0.7, 0.3, 0.0, 0.8, 0.2, 0.0, 0.9, 0.1, &
  & 0.0, 1.0, 0.0, 0.0, 1.0, 0.1, 0.0, 1.0, 0.2, 0.0, 1.0, 0.3, 0.0, 1.0, 0.4, &
  & 0.0, 1.0, 0.5, 0.0, 1.0, 0.6, 0.0, 1.0, 0.7, 0.0, 1.0, 0.8, 0.0, 1.0, 0.9, &
  & 1.0, 0.0, 1.0, 1.0, 0.1, 0.9, 1.0, 0.2, 0.8, 1.0, 0.3, 0.7, 1.0, 0.4, 0.6, &
  & 1.0, 0.5, 0.5, 1.0, 0.6, 0.4, 1.0, 0.7, 0.3, 1.0, 0.8, 0.2, 1.0, 0.9, 0.1, &
  & 1.0, 1.0, 0.0, 1.0, 1.0, 0.1, 1.0, 1.0, 0.2, 1.0, 1.0, 0.3, 1.0, 1.0, 0.4, &
  & 1.0, 1.0, 0.5, 1.0, 1.0, 0.6, 1.0, 1.0, 0.7, 1.0, 1.0, 0.8, 1.0, 1.0, 0.9, &
  & 1.0, 0.0, 1.0, 1.0, 0.1, 1.0, 1.0, 0.2, 1.0, 1.0, 0.3, 1.0, 1.0, 0.4, 1.0, &
  & 1.0, 0.5, 1.0, 1.0, 0.6, 1.0, 1.0, 0.7, 1.0, 1.0, 0.8, 1.0, 1.0, 0.9, 1.0  &
  /), [3, 50])

  select case (Action)
!
!
  case (1) !  Run one time only, to set up graphics initialization
!
!  Create graphics window frame to fill the whole screen
!
!kms    Status=GETWSIZEQQ(QWIN$FRAMEWINDOW,QWIN$SIZEMAX,FrameSize)
!kms    Status=SETWSIZEQQ(QWIN$FRAMEWINDOW,FrameSize)
!
!  Create  child window to hold text
!

    open (unit = 6, file = 'USER')  !! Active Window is text window
!kms    myscreen.numxpixels = int2(0.40*FrameSize.W)  ! Width of the text window
!kms    myscreen.numypixels = int2(0.75*FrameSize.H)  ! Height of the text window
!kms    myscreen.numtextcols=-1
!kms    myscreen.numtextrows=-1
!kms    myscreen.numcolors=-1
!kms    myscreen.fontsize=-1
!kms    myscreen.title = "Text "C ! Title of the graphics screen
!kms    status = SETWINDOWCONFIG(myscreen) ! Set configuration of the graphics window
!kms    Framesize.X = 0
!kms    Status=SETWSIZEQQ(6,FrameSize)
!
!  Background colors are set in six bytes: BBGGRR. White is FFFFFF and black is 000000.
!  Colow child window white
!
!kms    i=setbkcolorrgb(#FFFFFF)   !! white background
!kms    i=settextcolorrgb(#000000) !! black text
!kms    call clearscreen($GCLEARSCREEN)
!
!  Create child window to hold graphics
!
    open (unit = 10, file = 'USER')  !! Active Window is graphics window

!kms    myscreen.numxpixels = int2(0.60*FrameSize.W)   ! Width of the graphics window
!kms    myscreen.numypixels = int2(0.90*FrameSize.H)  ! Height of the graphics window
!kms    myscreen.numtextcols=-1
!kms    myscreen.numtextrows=-1
!kms    myscreen.numcolors=-1
!kms    myscreen.fontsize=-1
!kms    myscreen.title = "Program BZ "C ! Title of the graphics screen
!kms    status = SETWINDOWCONFIG(myscreen) ! Set configuration of the graphics window
!
!  Position child window for graphics
!
!kms    Framesize.TYPE = QWIN$SET
!kms    Framesize.X = 0.053*Framesize.W     ! Position graphics window to the right of the screen
!kms    top_left_x  = Framesize.X
!kms    top_left_y  = Framesize.Y
!kms    Status=SETWSIZEQQ(10,FrameSize)
!kms    i = SETBKCOLORRGB(Z'FFFFFF')
!kms    call clearscreen($GCLEARSCREEN)
!kms    Status = REGISTERMOUSEEVENT (10, MOUSE$LBUTTONDOWN, Details_of_cursor_point)
    xoffset = 70.0
    yoffset = 40.0
!kms    xscale  =  450.0*framesize.W/1920.0 ! Convert from non-graphics coordinates to graphics coordinates
!kms    yscale  =  450.0*framesize.H/1133.0 ! Convert from non-graphics coordinates to graphics coordinates

    pick_color = 0
!
!  Set colors used in lines as four-byte integers. 
!
!  Byte 1: Red
!  Byte 2: Green
!  Byte 3: Blue
!  Byte 4: not used
!
    do i = 1,50
      do l = 1,3
      j = nint(colors(l,i)*255)
      col(l) = j
      end do
      all_colors(i) = col(1) + 256*(col(2) + 256*col(3))
    end do      
!
! Construct part of format for text
!
!kms    i = nint((18*framesize.H)/1133.0)
    num1 = char(ichar("1") + i/10)
!kms    j = nint((10*framesize.W)/1920.0)
    num2 = char(ichar("1") + j/10)
!
! Set up fonts
!
!kms    i = INITIALIZEFONTS()
    return
!
!
  case (2) ! Move to new point, don't draw a line
!kms    i = SETACTIVEQQ(10)
!kms    i = FOCUSQQ(10)
    x = xx*xscale + xoffset
    y = yy*yscale + yoffset
!kms    CALL MOVETO(int2(x), int2(y), xy ) 
    return
!
!
  case (3) ! Move to new point drawing a line from the old point
    x = xx*xscale + xoffset
    y = yy*yscale + yoffset
!kms    i = LINETO(int2(x), int2(y)) 
    return
!
!
  case (6)  ! Run each time a new picture is drawn, initialize a picture.  
!kms    i = SETACTIVEQQ(10)
!kms    i = FOCUSQQ(10)
!kms    CALL SETVIEWORG( INT2(0),  INT2( 0 ), xy )
!kms    call clearscreen($GCLEARSCREEN)
!kms    i = SETCOLORRGB(Z'000000')
    pick_color = 0
    return
!
!
  case (96)  ! Write text to graph in Arial and Symbol
!kms    i = SETCOLORRGB(Z'000000')
    write(type_font,'(a,i'//num1//',a,i'//num2//',a)')"t'Arial'h" ,i, "w", j, "vb'"
    if (len_trim(line) == 3) then
      if (line(3:3) == "G") write(type_font,'(a,i'//num1//',a,i'//num2//',a)')"t'Symbol'h" ,i, "w", j, "vb'"
      if (line(3:3) == "G" .or. line(3:3) == "R")line(3:3) = " "
    end if
!kms    i = SETFONT(trim(type_font))
    x = xx*xscale + xoffset
    y = yy*yscale + yoffset
!kms    CALL MOVETO(int2(x), int2(y), xy )
!kms    call OUTGTEXT(trim(line))
    return
!
!
  case (97)  ! Set focus to channel 6

!kms    i = FOCUSQQ(6)
    return
!
!
  case (98)  ! Select black

!kms    i = SETACTIVEQQ(10)
!kms    i = FOCUSQQ(10)
!kms    i = SETCOLORRGB(Z'000000')
    return
!
!
    case (99)  ! Select color
    pick_color = pick_color + 1
    if (pick_color == 50) pick_color = 1 
!kms    i = SETCOLORRGB(all_colors(pick_color))
    return
!
!
  case (100)  ! Run one time only, to gracefully terminate the job
!kms    i = SETEXITQQ(QWIN$EXITNOPERSIST)
    return
  end select
END SUBROUTINE graphics