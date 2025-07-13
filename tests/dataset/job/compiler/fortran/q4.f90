program q4
  implicit none
  type :: InfoType
    integer :: id
    character(len=10) :: info
  end type InfoType
  type :: Keyword
    integer :: id
    character(len=32) :: word
  end type Keyword
  type :: TitleRec
    integer :: id
    character(len=32) :: title
    integer :: year
  end type TitleRec
  type :: MovieKeyword
    integer :: movie_id
    integer :: keyword_id
  end type MovieKeyword
  type :: MovieInfoIdx
    integer :: movie_id
    integer :: info_type_id
    character(len=4) :: info
  end type MovieInfoIdx
  type :: Row
    real(8) :: rating
    character(len=32) :: title
  end type Row
  type(InfoType) :: info_types(2)
  type(Keyword) :: keywords(2)
  type(TitleRec) :: titles(3)
  type(MovieKeyword) :: movie_keywords(3)
  type(MovieInfoIdx) :: movie_info_idxs(3)
  type(Row) :: rows(3)
  integer :: rc
  real(8) :: min_rating
  character(len=32) :: min_title
  character(len=32) :: s_rating
  character(len=256) :: out
  integer :: i,j,k,l,m
  logical :: first

  info_types(1) = InfoType(1,'rating')
  info_types(2) = InfoType(2,'other')

  keywords(1) = Keyword(1,'great sequel')
  keywords(2) = Keyword(2,'prequel')

  titles(1) = TitleRec(10,'Alpha Movie',2006)
  titles(2) = TitleRec(20,'Beta Film',2007)
  titles(3) = TitleRec(30,'Old Film',2004)

  movie_keywords(1) = MovieKeyword(10,1)
  movie_keywords(2) = MovieKeyword(20,1)
  movie_keywords(3) = MovieKeyword(30,1)

  movie_info_idxs(1) = MovieInfoIdx(10,1,'6.2')
  movie_info_idxs(2) = MovieInfoIdx(20,1,'7.8')
  movie_info_idxs(3) = MovieInfoIdx(30,1,'4.5')

  rc = 0
  do i=1,2
    if (trim(info_types(i)%info) == 'rating') then
      do j=1,3
        if (movie_info_idxs(j)%info_type_id == info_types(i)%id) then
          read(movie_info_idxs(j)%info,*) min_rating ! reuse variable
          if (min_rating > 5.0d0) then
            do k=1,3
              if (titles(k)%id == movie_info_idxs(j)%movie_id .and. titles(k)%year > 2005) then
                do l=1,3
                  if (movie_keywords(l)%movie_id == titles(k)%id) then
                    do m=1,2
                      if (keywords(m)%id == movie_keywords(l)%keyword_id .and. &
                          index(keywords(m)%word,'sequel') > 0 .and. &
                          movie_keywords(l)%movie_id == movie_info_idxs(j)%movie_id) then
                        rc = rc + 1
                        rows(rc)%rating = min_rating
                        rows(rc)%title = titles(k)%title
                      end if
                    end do
                  end if
                end do
              end if
            end do
          end if
        end if
      end do
    end if
  end do

  first = .true.
  do i=1,rc
    if (first) then
      min_rating = rows(i)%rating
      min_title = rows(i)%title
      first = .false.
    else
      if (rows(i)%rating < min_rating) min_rating = rows(i)%rating
      if (rows(i)%title < min_title) min_title = rows(i)%title
    end if
  end do

  call fmt_real(min_rating, s_rating, '(F0.1)')
  out = '[{"movie_title":"'//trim(min_title)//'","rating":"'//trim(s_rating)//'"}]'
  print '(A)', trim(out)
contains
  subroutine fmt_real(x, res, fmt)
    real(8), intent(in) :: x
    character(len=*), intent(out) :: res
    character(len=*), intent(in) :: fmt
    write(res, fmt) x
    res = trim(adjustl(res))
  end subroutine
end program q4
