program q5
  implicit none
  type :: CompanyType
    integer :: ct_id
    character(len=32) :: kind
  end type CompanyType
  type :: InfoType
    integer :: it_id
    character(len=32) :: info
  end type InfoType
  type :: TitleRec
    integer :: t_id
    character(len=32) :: title
    integer :: year
  end type TitleRec
  type :: MovieCompany
    integer :: movie_id
    integer :: company_type_id
    character(len=64) :: note
  end type MovieCompany
  type :: MovieInfo
    integer :: movie_id
    character(len=32) :: info
    integer :: info_type_id
  end type MovieInfo
  type(CompanyType) :: company_types(2)
  type(InfoType) :: info_types(1)
  type(TitleRec) :: titles(3)
  type(MovieCompany) :: movie_companies(3)
  type(MovieInfo) :: movie_infos(3)
  character(len=32) :: allowed(8)
  character(len=32) :: best_title
  logical :: first
  integer :: i,j,k,l,m,n

  company_types(1) = CompanyType(1,'production companies')
  company_types(2) = CompanyType(2,'other')

  info_types(1) = InfoType(10,'languages')

  titles(1) = TitleRec(100,'B Movie',2010)
  titles(2) = TitleRec(200,'A Film',2012)
  titles(3) = TitleRec(300,'Old Movie',2000)

  movie_companies(1) = MovieCompany(100,1,'ACME (France) (theatrical)')
  movie_companies(2) = MovieCompany(200,1,'ACME (France) (theatrical)')
  movie_companies(3) = MovieCompany(300,1,'ACME (France) (theatrical)')

  movie_infos(1) = MovieInfo(100,'German',10)
  movie_infos(2) = MovieInfo(200,'Swedish',10)
  movie_infos(3) = MovieInfo(300,'German',10)

  allowed = (/ 'Sweden','Norway','Germany','Denmark', &
               'Swedish','Denish','Norwegian','German' /)

  first = .true.
  do i=1,2
    if (trim(company_types(i)%kind) == 'production companies') then
      do j=1,3
        if (movie_companies(j)%company_type_id == company_types(i)%ct_id .and. &
            index(movie_companies(j)%note,'(theatrical)') > 0 .and. &
            index(movie_companies(j)%note,'(France)') > 0) then
          do k=1,3
            if (movie_infos(k)%movie_id == movie_companies(j)%movie_id) then
              do l=1,1
                if (movie_infos(k)%info_type_id == info_types(l)%it_id) then
                  do m=1,3
                    if (titles(m)%t_id == movie_companies(j)%movie_id .and. titles(m)%year > 2005) then
                      do n=1,8
                        if (trim(movie_infos(k)%info) == allowed(n)) then
                          if (first) then
                            best_title = titles(m)%title
                            first = .false.
                          else
                            if (titles(m)%title < best_title) best_title = titles(m)%title
                          end if
                          exit
                        end if
                      end do
                    end if
                  end do
                end if
              end do
            end if
          end do
        end if
      end do
    end if
  end do

  print '(A)', '[{"typical_european_movie":"'//trim(best_title)//'"}]'
end program q5
