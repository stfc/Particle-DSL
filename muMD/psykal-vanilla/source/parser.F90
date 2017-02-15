!    Copyright (c) 2006-2016 Alin Marin Elena <alinm.elena@gmail.com>
!    The MIT License http://opensource.org/licenses/MIT

!> \brief deals with all subroutines related with the parsing
!> \author Alin M. Elena
!> \date 14th of January 2006
!> \remark 17th of July 2007 removed the save attribute from variables
!> \remark July 2016 extended and adapted 
!> \internal suspected memory leak
module m_Parser
  use m_Constants
  use m_Useful
  use m_Types
  implicit none
  private
  ! equalize lengths by space padding at the end
  character(len=*),parameter :: allowedDups(3) = ['io   ','no   ','print']
  public :: ParseFile, EndParse
  public :: GetLogical, GetString, GetReal, GetInteger,GetLine, GetBlock
!
!> data structure of a node in the parsing tree
  type, private :: names
    character (len=k_mw)  :: nam = '' !< name of the entity read in file
    character (len=k_ml)  :: value = '' !< the value associated with the node
    Logical               :: processed = .false.
    integer :: ut=0 !< unit number where write the info to be parsed (write(ut,*))
    integer :: lines = 0
    type (names), pointer :: next => null ()!< next element in tree
  end type names
!
!> data structure of info about the parsed files
  type, private :: infoParseType
    integer :: comments = 0 !< number of comments parsed
    integer :: empty = 0 !< number of empty lines
    integer :: tokens = 0 !< number of tokens
    integer :: lines = 0 !< number of lines
    integer :: blocks = 0 !< number of blocks
    integer :: blocklines = 0 !< number of lines in blocks
  end type infoParseType
  type (infoParseType) :: ireport
!
  type (names), pointer :: tnames,currentt !< blocks and tokens lists for the files plus the current ones
  type (names), pointer :: bnames,currentb !< blocks and tokens lists for the files plus the current ones
!
contains
!>  \brief opens the input and error files
!>  \details allocates the lists for blocks and tokens
!>  starts the parsing
!> \author Alin M. Elena (Belfast)
!> \date 14th-15th of January 2006
!> \param ioLoc is type(ioType) (see m_Types::ioType)
!> \remark 20th of January 2007 added the parameter \em ioLoc
  subroutine ParseFile (ioLoc,uinp,warm)
    character (len=*), parameter :: myname = 'ParseFile'
    type (ioType), intent (inout) :: ioLoc
    integer, intent (in) :: uinp
    integer,intent(in), optional :: warm 

    character(len=k_ml) :: nam
    integer :: errno,iu

    ireport%empty=0
    ireport%comments=0
    ireport%tokens=0
    ireport%lines=0
    ireport%blocks=0
    ireport%blocklines=0
!
!allocate the lists for name of the blocks and tokens
    allocate (tnames)
    allocate (bnames)

    call parse (uinp, ioLoc)
    if (present(warm)) then 
      iu=ioLoc%udeb
    else
      iu=ioLoc%uerr
    endif
!
    inquire (unit=uinp, name=nam)
    write(iu,*)
    write (iu, '(a42)') repeat('=',42)
    write (iu, '(a)') "Parsing report for file: "//trim(nam)
    write (iu, '(a)') "1. Token labels"
    call PrintName (tnames, iu)
    write (iu, '(a)') "2. Block labels"
    call PrintName (bnames, iu)
    write (iu, '(a)') "3. General info"
    write (iu, '(a,i5)') "No of lines read", ireport%lines
    write (iu, '(a,i5)') "No of comment lines", ireport%comments
    write (iu, '(a,i5)') "No of empty lines", ireport%empty
    write (iu, '(a,i5)') "No of tokens", ireport%tokens
    write (iu, '(a,i5)', advance='no') "No of blocks", ireport%blocks
    write (iu, '(a,i5,a)') " containing ", ireport%blocklines, " lines"
    write (iu, '(a42)') repeat('=',42)
  end subroutine ParseFile
!
!> \brief    recursive subroutine which effectively parses the file
!> \details    associated with unit=nounit
!>          if an include statement is found the routine is called again to parse the new file
!> \author Alin M. Elena (Belfast)
!> \date 14th-15th of January 2006
!> \param nounit integer, represents the unit number of a file which was previous opened and now
!> is parsed
!> \param  ioLoc type(ioType) (see ioType)
!> \param  ireport type(infoParseType),  keeps the info about the parsed files (see m_Types::infoParseType type)
!> \remark 20th of January, 2007, by Alin M Elena (Queen's University Belfast), added parameter \em ioLoc
!
  subroutine parse (nounit, ioLoc)
    character (len=*), parameter :: myname = 'parse'
    type (ioType), intent (inout) :: ioLoc
    integer, intent (in) :: nounit
!
    character (len=k_ml) :: line, lineaux, storeline, lineaux2, blockfile, nam
    character (len=k_mw) :: tokename,blockname,endblockname
    integer :: errno, noinc, lineno, nt,blines
    logical :: ex, op, bool
!
    lineno = 0
    inquire (unit=nounit, name=nam)
    do while (GetLine(nounit, line))
      lineaux = adjustl (line)
      lineno = lineno + 1
      if (lineaux(1:1) == "!" .or. lineaux(1:1) == "#" .or. lineaux(1:2) == "//") then
        ireport%comments = ireport%comments + 1
! just count the comments nothing else
      else if (len(trim(line)) == 0) then
        ireport%empty = ireport%empty + 1
      else if (cstr(lineaux(1:5), "block")) then
      ! defines the behaviour of a block - endblock statement
        ireport%blocks = ireport%blocks + 1
        read (lineaux(6:k_ml),*, iostat=errno) blockname
        if (errno /= 0) then 
          call ParseErr ("invalid name after block statement", myname, nam, lineno, ioLoc)
        endif 
        bool = .true.
        blines = 0
        blockfile=trim(blockname)//".blk"
      !  create a scratch file
        open (newunit=nt, file=trim(blockfile),status="replace", action="write")
        do while (bool)
          bool = GetLine (nounit, line)
          lineno = lineno + 1
          lineaux = adjustl (line)
        ! take some action if we have a endblock
          if (cstr(lineaux(1:8), "endblock")) then
          ! check the name
            read (lineaux(9:k_ml),*, iostat=errno) endblockname
            if (errno /= 0) then 
              call ParseErr ("invalid name after endblock statement", myname, nam, lineno, ioLoc)
            endif
            ! closing the wrong block
            if ( .not. cstr(trim(endblockname), trim(blockname))) then 
              call ParseErr ("closing wrong block, expected endblock "//&
               & trim(blockname)//" found endblock "//trim(endblockname), myname, nam, lineno, ioLoc)
            endif
            exit
          end if
        ! parse the content of the block line by line and get rid of commented and empty lines
          if (lineaux(1:1) == "!" .or. lineaux(1:1) == "#" .or. lineaux(1:2) == "//") then
            ireport%comments = ireport%comments + 1
          else if (len(trim(line)) == 0) then
            ireport%empty = ireport%empty + 1
          else
        ! count only not obviuos without value lines
        ! get rid of any inline comment
            storeline = ParseLine (lineaux)
            write (nt, '(a)') trim (storeline)
            blines = blines + 1
          end if
        end do
    ! check if we have reached end-of-file
        if ( .not. bool) call ParseErr ("missing endblock "//trim(blockname), myname, nam,-1, ioLoc)
    ! check empty blocks and give an warning, empty blocks are ignored
        if (blines == 0) then
          call ParseWar ("detected empty(probably only comments and empty lines) block "//trim(blockname), myname, nam, lineno, &
          & ioLoc)
      ! empty files we delete them immediately
        end if
    ! check the uniquness of the block
        ireport%blocklines = blines + ireport%blocklines
    !
        close (nt)
    ! check for the block in the existent list
        if (ireport%blocks == 1) then
          currentb => bnames
          bnames%nam = trim (blockname)
          bnames%lines = blines
          bnames%value = trim (blockfile)
        else
          if (FindName(trim(blockname), bnames)) call ParseErr ("found block "//trim(blockname)//" duplicated", myname, nam, &
            & lineno, ioLoc)
          call AddName (trim(blockname), currentb, blines)
          currentb%value = trim (blockfile)
      end if
    else if (cstr(lineaux(1:8), "endblock")) then
    ! endblock without block
      call ParseErr ("endblock without block", myname, nam, lineno, ioLoc)
    else
      ireport%tokens = ireport%tokens + 1
      call getToken(lineaux,tokename,storeline)
! check for the token in the existent list and add it if is new
        if (ireport%tokens == 1) then
          currentt => tnames
          tnames%nam = "title"
          If (cstr(trim(tokename),"title")) then
            tnames%value = trim(storeline)
          Else
            tnames%value = trim(lineaux)
          End If
        else
          If (FindName(trim(tokename), tnames)) then
            call ParseErr ("found token "//trim(tokename)//" duplicated", myname, nam, lineno,ioLoc)
          End If
          call AddName (trim(tokename), currentt)
          currentt%value = trim (storeline)
        end if
        if (cstr(trim(tokename),"finish")) exit
      end if
    end do
    ireport%lines = ireport%lines + lineno
  end subroutine parse

  Subroutine getToken(lineaux,tokename,storeline)
    character (len=*), parameter :: myname = 'getToken'
    character (len=*), intent (in)  :: lineaux
    character (len=*), intent (out) :: tokename
    character (len=*), intent (out) :: storeline
    character (len=k_ml) :: lineaux2
    character (len=k_mw) :: t2
    Integer   :: errno


    read (lineaux,*, iostat=errno) tokename
    lineaux2 = ParseLine (lineaux)
    If (isInList(tokename,allowedDups)) then
      read (lineaux2(len(trim(tokename))+1:k_ml),*, iostat=errno) t2 
      tokename=trim(tokename)//" "//trim(t2)
    End If
    read (lineaux2(len(trim(tokename))+1:k_ml),*, iostat=errno) storeline
    if (errno /= 0) storeline = ''
  End Subroutine getToken

  Logical Function isInList(token,list)
    character (len=*), parameter :: myname = 'isInList'
    character (len=*), intent (in) :: token
    character (len=*), intent (in) :: list(:)

    integer :: i

    isInList = .false.
    Do i=1,size(list)
      If (cstr(trim(token), trim(list(i)))) Then
        isInList=.true.
        Exit
      End If
    End Do
  End Function isInList

!
!> \brief logical function reads a line from a unit
!> \details
!> \author Alin M Elena
!> \date 14th of January 2006
!> \param uno integer, unit number from where to read the line
!> \param line character(len=*), the line that was read
!> \return .true. if successfull in reading the line, .false. otherwise
!> \remarks
!
  logical function GetLine (uno, line)
    character (len=*), parameter :: myname = 'GetLine'
    character (len=k_ml), intent (out) :: line
    integer, intent (in) :: uno
    integer :: errno
!
    inquire (unit=uno, iostat=errno)
    GetLine = .false.
    if (errno /= 0) then
      write (*,*) "Unexpected error opening the input file(s)"
      stop
    end if
    read (uno, fmt='(a)', iostat=errno) line
    if (errno == 0) GetLine = .true.
  end function GetLine
!
!> \brief adds a new node in the list
!> \details
!> \author Alin M Elena
!> \date 14th of January 2006
!> \param word character(len=*), value for field nam of the node
!> \param current pointer to the last node in the list
!> \param lines integer, optional value for field lines
!
  subroutine AddName (word, current,lines)
    character (len=*), parameter :: myname = 'AddName'
    character (len=*), intent (in) :: word
    integer, intent (in), optional :: lines

    type (names), pointer :: current
    type (names), pointer :: node
!
    allocate (node)
    node%nam = trim (word)
    current%next => node
    if (present(lines)) node%lines = lines
    current => node
!
  end subroutine AddName
!
!> \brief logical function finds a field in a list starting at root
!> \details
!> \author Alin M Elena
!> \date 14th of January 2006
!> \param word character(len=*), value of the field nam to be searched for
!> \param root type(names), pointer, starting point in search
!> \param loc tppe(names), pointer, optional returns the location where the info was found.
  logical function FindName (word, root, loc)
    character (len=*), parameter :: myname = 'FindName'
    character (len=*), intent (in) :: word
    type (names), pointer :: root
    type (names), pointer, optional :: loc
    type (names), pointer :: current
!
    current => root
    FindName = .false.
    do while (associated(current))
      if (cstr(trim(word), trim(current%nam))) then
        FindName = .true.
        if (present(loc)) loc => current
        exit
      end if
      current => current%next
    end do
  end function FindName
!
!> \brief Prints at a specified unit the nam field  from a list
!> \details
!> \author Alin M Elena
!> \date 14th of January 2006
!> \param root type(names), pointer  starting node in the Printing list
!> \param ioLoc type(ioType) conatins the unit to Print (see m_Types::ioType)
!> \remarks
  subroutine PrintName (root, ounit)
    character (len=*), parameter :: myname = 'PrintName'
    type (names), pointer :: root
    integer, intent(in) :: ounit

    type (names), pointer :: current
!
    current => root
    do while (associated(current))
      write (ounit, '(a)') trim (current%nam)
      current => current%next
    end do
  end subroutine PrintName
!
!> \brief recursive function that deallocates all the nodes starting with root
!> \details
!> \author Alin M Elena
!> \date 14th of January 2006
!> \param root type(names), pointer the starting node
!> \remarks
  recursive subroutine DeleteList (root)
    character (len=*), parameter :: myname = 'DeleteList'
    type (names), pointer :: root
    type (names), pointer :: current
!
    current => root%next
    if (associated(current)) then
      call DeleteList (current)
    else
      if (root%ut/=0) close(root%ut)
      deallocate (root)
    end if
  end subroutine DeleteList
!
!> \brief parses a line removing comments
!> \details everything after ! \# or // is considered a comment
!> \author Alin M Elena
!> \date 14th of January 2006
!> \param line character(len=*) the line to be parsed
  character (k_ml) function ParseLine (line)
    character (len=*), parameter :: myname = 'ParseLine'
    character (len=*), intent (in) :: line
    integer :: p (1:3), pos
!
    p (1) = scan (line, "!")
    p (2) = scan (line, "#")
    p (3) = index (line, "//")
    where (p == 0) p = k_ml + 1
    pos = minval (p) - 1
    if (pos /= k_ml) then
      ParseLine = line (1:pos)
    else
      ParseLine = line
    end if
  end function ParseLine
!> \brief Prints an error message and aborts the parsing process
!> \details
!> \author Alin M Elena
!> \date 14th of January 2006
!> \param message the error message
!> \param routine the subprogram that generated the error
!> \param filename raeding this \em filename the error occured
!> \param lineno the line number that generated the error
!> \param ioLoc type(ioType) (see m_Types::ioType)
  subroutine ParseErr (message, routine, filename, lineno, io)
    character (len=*), parameter :: myname = 'ParseErr'
    character (len=*), intent (in) :: message, routine, filename
    integer, intent (in) :: lineno
    type (ioType), intent (inout) :: io
!
    write (io%uerr, '(a,a)') "Error: ", trim (message)
    write (io%uerr, '(a,a)') "Routine: ", trim (routine)
    write (io%uerr, '(a,a)') "File: ", trim (filename)
    write (io%uerr, '(a,i7)') "Line number: ", lineno
    write (io%uerr, '(a)') "User stop"
    write (*, '(a,a)') "User stop, but probably this is not what you want, check file: ", io%errFile
    stop
  end subroutine ParseErr
!
!> \brief Prints a warning message occured in the parsing process
!> \details
!> \author Alin M Elena
!> \date 14th of January 2006
!> \param message the error message
!> \param routine the subprogram that generated the warning
!> \param filename raeding this \em filename the warning occured
!> \param lineno the line number that generated the warning
!> \param ioLoc type(ioType) (see m_Types::ioType)
!
  subroutine ParseWar (message, routine, filename, lineno, io)
    character (len=*), parameter :: myname = 'ParseWar'
    character (len=*), intent (in) :: message, routine, filename
    integer, intent (in) :: lineno
    type (ioType), intent (inout) :: io
!
    write (io%uerr, '(a,a)') "Warning: ", trim (message)
    write (io%uerr, '(a,a)') "Routine: ", trim (routine)
    write (io%uerr, '(a,a)') "File: ", trim (filename)
    write (io%uerr, '(a,i7)') "Line number: ", lineno
    write (io%uerr, '(a)') "User warning"
    write (*, '(a,a)') "User warning, check file: ", trim (io%errFile)
  end subroutine ParseWar
!
!> \brief  returns a logical value found in the input file(s) associated with the token \em label
!> \details  if no token is found the value is set to dflt
!> default value is set to .true. if no dflt parameter is present
!> output is put in the units indicated by \em ioLoc
!> \author Alin M Elena
!> \date 14th of January 2006
!> \param label character(len=*), the token to search for
!> \param dflt optional default value for the token \em label
!> \param ioLoc type(ioType) (see m_Types::ioType)
!> \remarks 20th of January, 2007, by Alin M Elena (Queen's University Belfast), added \em ioLoc parameter
  logical function GetLogical (io, label, dflt)
    character (len=*), parameter :: myname = 'GetLogical'
    character (len=*), intent (in) :: label
    type (ioType), intent (in) :: io
    logical, intent (in), optional :: dflt
    type (names), pointer :: found
    character(len=10) :: def
!default value is set to .true. if no dflt parameter is present
!
    def=''
    if (FindName(trim(label), tnames, found)) then
      if ((cstr(trim(found%value), "yes")) .or. (cstr(trim(found%value), "true")) .or. (cstr(trim(found%value), ".true.")) .or. &
     & (cstr(trim(found%value), "t")) .or. (cstr(trim(found%value), "y")) .or. (cstr(trim(found%value), "1"))) then
        GetLogical = .true.
      else if ((cstr(trim(found%value), "no")) .or. (cstr(trim(found%value), "false")) .or. (cstr(trim(found%value), ".false.")) &
     & .or. (cstr(trim(found%value), "f")) .or. (cstr(trim(found%value), "n")) .or. (cstr(trim(found%value), "0"))) then
        GetLogical = .false.
      elseif (len_trim(found%value)==0) then
        GetLogical = .true.
      else
        call error (__FILE__,__LINE__,-105,&
          "Unsuported value: "//trim(label)//" "//trim(found%value), io)
      end if
      found%processed=.true.
    else
      if (present(dflt)) then
        GetLogical = dflt
      else
        GetLogical = .true.
      end if
      def="! default"
    end if
    write (io%udeb, '(a,2x,l1,a)') trim (label), GetLogical,trim(def)
  end function GetLogical
!
!> \brief  returns a string value found in the input file(s) associated with the token \em label
!> \details  if no token is found the value is set to dflt
!> default value is set to empty string if no dflt parameter is present
!> output is put in the units indicated by \em ioLoc
!> \author Alin M Elena
!> \date 14th of January 2006
!> \param label character(len=*), the token to search for
!> \param dflt optional default value for the token \em label
!> \param ioLoc type(ioType) (see m_Types::ioType)
!> \param Print optional logical determines if the token is Printed or not. if the parameter is missing is assuk_med .false.
!> \remarks 20th of January, 2007, by Alin M Elena (Queen's University Belfast), added \em ioLoc parameter
  character (len=k_ml) function GetString (io, label, dflt, Print)
    character (len=*), parameter :: myname = 'GetString'
    type (ioType), intent (in) :: io
    character (len=*), intent (in) :: label
    character (len=*), intent (in), optional :: dflt
    logical, intent (in), optional :: Print
    type (names), pointer :: found
    character(len=10) :: def
! default value if no dflt parameter is present is empty string
! so be sure that you provide a default parameter
    def=''
    if (FindName(trim(label), tnames, found)) then
      GetString = trim(found%value)
      found%processed=.true.
    else
      if (present(dflt)) then
        GetString = trim (dflt)
      else
        GetString = ''
      end if
      def='! default'
    end if
    if (present(print)) Then
      if (print) Then
        write(io%udeb,'(a,2x,a,1x,a)') trim(label), trim(GetString),trim(def)
      End If
    End If
  end function GetString
!
!> \brief  returns a real(kind=pr) value found in the input file(s) associated with the token \em label
!> \details  if no token is found the value is set to dflt
!> default value is set to 0.0_pr if no dflt parameter is present
!> output is put in the units indicated by \em ioLoc
!> \author Alin M Elena
!> \date 14th of January 2006
!> \param label character(len=*), the token to search for
!> \param dflt optional default value for the token \em label
!> \param ioLoc type(ioType) (see m_Types::ioType)
!> \remarks 20th of January, 2007, by Alin M Elena (Queen's University Belfast), added \em ioLoc parameter
  real (rp) function GetReal (ioLoc, label, dflt)
    character (len=*), parameter :: myname = 'GetReal'
    type (ioType), intent (inout) :: ioLoc
    character (len=*), intent (in) :: label
    real (rp), intent (in), optional :: dflt
    type (names), pointer :: found
    character (len=k_mw) :: aux
    integer :: errno
    character(len=10) :: def
! default value if no dflt parameter is present is 0.0_pr
    def=''
    if (FindName(trim(label), tnames, found)) then
      aux = trim (found%value)
      read (aux,*, iostat=errno) GetReal
      if (errno /= 0) call error (__FILE__,__LINE__,-104,&
        "wrong value "//trim(found%value)//" suplied for label "//trim(label), ioLoc)
      found%processed=.true.
    else
      if (present(dflt)) then
        GetReal = dflt
      else
        GetReal = 0.0_rp
      end if
      def='! default'
    end if
    write (ioLoc%udeb, '(a,2x,g32.16,1x,a)') trim (label), GetReal, trim(def)
  end function GetReal
!
!> \brief  returns an integer value found in the input file(s) associated with the token \em label
!> \details  if no token is found the value is set to dflt
!> default value is set to 0 if no dflt parameter is present
!> output is put in the units indicated by \em ioLoc
!> \author Alin M Elena
!> \date 14th of January 2006
!> \param label character(len=*), the token to search for
!> \param dflt optional default value for the token \em label
!> \param ioLoc type(ioType) (see m_Types::ioType)
!> \remarks 20th of January, 2007, by Alin M Elena (Queen's University Belfast), added \em ioLoc parameter
  integer function GetInteger (io, label, dflt)
    character (len=*), parameter :: myname = 'GetInteger'
    type (ioType), intent (inout) :: io
    character (len=*), intent (in) :: label
    integer, intent (in), optional :: dflt
    type (names), pointer :: found
    character (len=k_mw) :: aux
    integer :: errno
    character(len=10) :: def
! default value if no dflt parameter is present is 0
!
    def=''
    if (FindName(trim(label), tnames, found)) then
      aux = trim (found%value)
      read (aux,*, iostat=errno) GetInteger
      if (errno /= 0) call error (__FILE__,__LINE__,-103,&
        "wrong value "//trim(found%value)//" suplied for label "//trim(label), io)
      found%processed=.true.
    else
      if (present(dflt)) then
        GetInteger = dflt
      else
        GetInteger = 0
      end if
      def='! default'
    end if
    write (io%udeb, '(a,2x,i0,a)') trim (label), GetInteger,trim(def)
  end function GetInteger
  
  !> \brief  returns a logical value to indicate if it found in the input file(s) a valid block associated with the token \em label
  !> \details  if no token is found the value returned is .false.
  !> a unit from where the content of the block cand be read is returned via nt output is put in the units indicated by ioLoc
  !> \author Alin M Elena
  !> \date 14th of January 2006
  !> \param label character(len=*), the token to search for
  !> \param nt unit number from where to read the block data
  !> \param ioLoc type(ioType) (see m_Types::ioType)
  !> \remarks 20th of January, 2007, by Alin M Elena (Queen's University Belfast), added \em ioLoc parameter
  logical function GetBlock (ioLoc, label, nt)
    character (len=*), parameter :: myname = 'GetBlock'
    type (ioType), intent (in) :: ioLoc
    character (len=*), intent (in) :: label
    integer, intent (out) :: nt
    type (names), pointer :: found
    character (len=k_mw) :: line
    !
    if (FindName(trim(label), bnames, found)) then
      GetBlock = .true.
      line = found%value
      open (newunit=nt, action="read", status="old", file=trim(line))
      found%ut=nt
      found%processed=.true.
      write (ioLoc%udeb, '(a,2x,l1)') trim (label), GetBlock
    else
      write (ioLoc%udeb, '(a)') "Block " // trim (label) // " was not found"
      GetBlock = .false.
    end if
  end function GetBlock
  !
  !> \brief ends the parsing process and cuts the \em tree (deallocates the memory used during the parsing process)

  integer function countRead (root)
    character (len=*), parameter :: myname = 'countRead'
    type (names), pointer :: root

    type (names), pointer :: current
    integer :: c
!
    current => root
    c=0
    do while (associated(current))
      if (current%processed) c=c+1
      current => current%next
    end do
    countRead=c
  end function countRead

  subroutine printUnprocessed (root,io)
    character (len=*), parameter :: myname = 'printUnprocessed'
    type (names), pointer :: root
    type(ioType),intent(in) :: io

    type (names), pointer :: current
!
    write(io%uerr,'(a)')"Unprocessed tokens: "
    current => root
    do while (associated(current))
      if (.not. current%processed) then
        write(io%uerr,'(a)')trim(current%nam)
      End If
      current => current%next
    end do
  end subroutine printUnprocessed

!> \author Alin M Elena
!> \date 14th of January 2006
!> \warning after the call to this function none of the get_* function will work
!> all the input has to be done before the call to it.
  subroutine EndParse(io)
    character (len=*), parameter :: myname = 'EndParse'
    type (ioType), intent (inout) :: io
! to be called only when there is nothing to be read
    type (names), pointer :: current
    integer :: nt
    character (len=k_mw) :: filename
    If (countRead(tnames)/= ireport%tokens) Then
      write(io%uerr,'(a)')"***Unprocessed tokens found: "
      call printUnprocessed(tnames,io)
      call error (__FILE__,__LINE__,-102,&
        "Unprocessed tokens exist. Check "//trim(io%errFile)//"!!!", io)
    Else
      write(io%udeb,'(a)')"***All found tokens processed"
    End If
    call DeleteList (tnames)
    ! check for unprocessed blocks and go mad if any found
    If (countRead(bnames)/= ireport%blocks) Then
      write(io%uerr,'(a)')"***Unprocessed blocks found: "
      call printUnprocessed(bnames,io)
      call error (__FILE__,__LINE__,-110,&
        "Unprocessed blocks exist. Check "//trim(io%errFile)//"!!!", io)
    Else
      write(io%udeb,'(a)')"***All found blocks processed"
    End If
    call DeleteList (bnames)
  end subroutine EndParse
!
end module m_Parser
