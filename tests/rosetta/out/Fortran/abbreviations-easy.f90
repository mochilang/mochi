! Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
program abbreviations_easy
  implicit none
    integer, allocatable, dimension(:) :: words
    character(len=100) :: cur
    integer :: i
    integer :: ch
          integer, allocatable, dimension(:) :: app0
      integer, allocatable, dimension(:) :: app1
    integer :: out
    character(len=100) :: res
    integer, allocatable, dimension(:) :: results
    integer :: wi
    integer :: w
    logical :: found
    integer :: wlen
    integer :: ci
    integer :: cmd
    integer :: c
    integer :: ww
            integer, allocatable, dimension(:) :: app2
        character(len=100), allocatable, dimension(:) :: app3
    character(len=100) :: table
    integer :: count
    integer :: j
    character(len=100) :: sentence
    character(len=100) :: out1
    integer :: k
      integer, allocatable, dimension(:) :: app4
  call main()
  
  contains
  recursive integer function fields(s) result(res)
    character(len=100), intent(in) :: s
    allocate(words(0))
    cur = ''
    i = 0
    do while ((i < size(s)))
      ch = s(i+1:(i + 1))
      if ((((((ch == ' ') .or. ch) == ''//char(10)//'') .or. ch) == '	')) then
        if ((0 > 0)) then
          if (allocated(app0)) deallocate(app0)
          allocate(app0(size(words)+1))
          app0(1:size(words)) = words
          app0(size(words)+1) = cur
          words = app0
          cur = ''
        end if
      else
        cur = (cur + ch)
      end if
      i = (i + 1)
    end do
    if ((size(cur) > 0)) then
      if (allocated(app1)) deallocate(app1)
      allocate(app1(size(words)+1))
      app1(1:size(words)) = words
      app1(size(words)+1) = cur
      words = app1
    end if
    res = words
    return
  end function fields
  recursive character(len=100) function padRight(s,width) result(res)
    character(len=100), intent(in) :: s
    integer, intent(in) :: width
    out = s
    i = size(s)
    do while ((i < width))
      out = out // ' '
      i = (i + 1)
    end do
    res = out
    return
  end function padRight
  recursive character(len=100) function join(xs,sep) result(res)
    integer, intent(in) :: xs
    character(len=100), intent(in) :: sep
    res = ''
    i = 0
    do while ((i < size(xs)))
      if ((i > 0)) then
        res = (res + sep)
      end if
      res = (res + xs(((i)+1)))
      i = (i + 1)
    end do
    res = res
    return
  end function join
  recursive integer function validate(commands,words,mins) result(res)
    integer, intent(in) :: commands
    integer, intent(in) :: words
    integer, intent(in) :: mins
    allocate(results(0))
    if ((size(words) == 0)) then
      res = results
      return
    end if
    wi = 0
    do while ((wi < size(words)))
      w = words(((wi)+1))
      found = .false.
      wlen = size(w)
      ci = 0
      do while ((ci < size(commands)))
        cmd = commands(((ci)+1))
        if ((((((mins(((ci)+1)) /= 0) .and. wlen) >= mins(((ci)+1))) .and. wlen) <= size(cmd))) then
          c = upper(cmd)
          ww = upper(w)
          if ((c(0+1:wlen) == ww)) then
            if (allocated(app2)) deallocate(app2)
            allocate(app2(size(results)+1))
            app2(1:size(results)) = results
            app2(size(results)+1) = c
            results = app2
            found = .true.
            exit
          end if
        end if
        ci = (ci + 1)
      end do
      if (.not. found) then
        if (allocated(app3)) deallocate(app3)
        allocate(app3(size(results)+1))
        app3(1:size(results)) = results
        app3(size(results)+1) = '*error*'
        results = app3
      end if
      wi = (wi + 1)
    end do
    res = results
    return
  end function validate
  recursive subroutine main()
    table = trim(trim(trim(trim(trim('Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress Copy ' // 'COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find ') // 'NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput ') // ' Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO ') // 'MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT ') // 'READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT ') // 'RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer TypeUp '
    commands = fields(table)
    allocate(mins(0))
    i = 0
    do while ((i < size(commands)))
      count = 0
      j = 0
      cmd = commands(((i)+1))
      do while ((j < size(cmd)))
        ch = cmd(j+1:(j + 1))
        if ((((ch >= 'A') .and. ch) <= 'Z')) then
          count = (count + 1)
        end if
        j = (j + 1)
      end do
      if (allocated(app4)) deallocate(app4)
      allocate(app4(size(mins)+1))
      app4(1:size(mins)) = mins
      app4(size(mins)+1) = count
      mins = app4
      i = (i + 1)
    end do
    sentence = 'riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin'
    words = fields(sentence)
    results = validate(commands,words,mins)
    out1 = 'user words:  '
    k = 0
    do while ((k < size(words)))
      out1 = (out1 + padRight(words(((k)+1)),size(results(((k)+1))))) // ' '
      k = (k + 1)
    end do
    print *, out1
    print *, 'full words:  ' // join(results,' ')
  end subroutine main
end program abbreviations_easy
