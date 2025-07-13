program q3
  implicit none
  type :: Keyword
    integer :: id
    character(len=32) :: word
  end type Keyword
  type :: MovieInfo
    integer :: movie_id
    character(len=32) :: info
  end type MovieInfo
  type :: MovieKeyword
    integer :: movie_id
    integer :: keyword_id
  end type MovieKeyword
  type :: TitleRec
    integer :: id
    character(len=32) :: title
    integer :: year
  end type TitleRec
  type(Keyword) :: keywords(2)
  type(MovieInfo) :: movie_infos(3)
  type(MovieKeyword) :: movie_keywords(4)
  type(TitleRec) :: titles(3)
  character(len=32) :: allowed(8)
  character(len=32) :: best_title
  logical :: first
  integer :: i,j,k,l,m,n

  keywords(1) = Keyword(1,'amazing sequel')
  keywords(2) = Keyword(2,'prequel')

  movie_infos(1) = MovieInfo(10,'Germany')
  movie_infos(2) = MovieInfo(30,'Sweden')
  movie_infos(3) = MovieInfo(20,'France')

  movie_keywords(1) = MovieKeyword(10,1)
  movie_keywords(2) = MovieKeyword(30,1)
  movie_keywords(3) = MovieKeyword(20,1)
  movie_keywords(4) = MovieKeyword(10,2)

  titles(1) = TitleRec(10,'Alpha',2006)
  titles(2) = TitleRec(30,'Beta',2008)
  titles(3) = TitleRec(20,'Gamma',2009)

  allowed = (/ 'Sweden','Norway','Germany','Denmark', &
               'Swedish','Denish','Norwegian','German' /)

  first = .true.
  do i=1,2
    if (index(keywords(i)%word,'sequel') > 0) then
      do j=1,4
        if (movie_keywords(j)%keyword_id == keywords(i)%id) then
          do k=1,3
            if (movie_infos(k)%movie_id == movie_keywords(j)%movie_id) then
              do l=1,3
                if (titles(l)%id == movie_infos(k)%movie_id .and. titles(l)%year > 2005) then
                  if (movie_keywords(j)%movie_id == movie_infos(k)%movie_id) then
                    do m=1,8
                      if (trim(movie_infos(k)%info) == allowed(m)) then
                        if (first) then
                          best_title = titles(l)%title
                          first = .false.
                        else
                          if (titles(l)%title < best_title) best_title = titles(l)%title
                        end if
                        exit
                      end if
                    end do
                  end if
                end if
              end do
            end if
          end do
        end if
      end do
    end if
  end do

  print '(A)', '[{"movie_title":"'//trim(best_title)//'"}]'
end program q3
