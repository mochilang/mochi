program q2
  implicit none
  type :: CompanyName
    integer :: id
    character(len=10) :: country
  end type CompanyName
  type :: Keyword
    integer :: id
    character(len=32) :: word
  end type Keyword
  type :: MovieCompany
    integer :: movie_id
    integer :: company_id
  end type MovieCompany
  type :: MovieKeyword
    integer :: movie_id
    integer :: keyword_id
  end type MovieKeyword
  type :: TitleRec
    integer :: id
    character(len=32) :: title
  end type TitleRec
  type(CompanyName) :: company_names(2)
  type(Keyword) :: keywords(2)
  type(MovieCompany) :: movie_companies(2)
  type(MovieKeyword) :: movie_keywords(2)
  type(TitleRec) :: titles(2)
  character(len=32) :: best_title
  logical :: first
  integer :: i,j,k,l,m

  company_names(1) = CompanyName(1,'[de]')
  company_names(2) = CompanyName(2,'[us]')

  keywords(1) = Keyword(1,'character-name-in-title')
  keywords(2) = Keyword(2,'other')

  movie_companies(1) = MovieCompany(100,1)
  movie_companies(2) = MovieCompany(200,2)

  movie_keywords(1) = MovieKeyword(100,1)
  movie_keywords(2) = MovieKeyword(200,2)

  titles(1) = TitleRec(100,'Der Film')
  titles(2) = TitleRec(200,'Other Movie')

  first = .true.
  do i=1,2
    if (trim(company_names(i)%country) == '[de]') then
      do j=1,2
        if (movie_companies(j)%company_id == company_names(i)%id) then
          do k=1,2
            if (titles(k)%id == movie_companies(j)%movie_id) then
              do l=1,2
                if (movie_keywords(l)%movie_id == titles(k)%id) then
                  do m=1,2
                    if (keywords(m)%id == movie_keywords(l)%keyword_id .and. &
                        trim(keywords(m)%word) == 'character-name-in-title' .and. &
                        movie_companies(j)%movie_id == movie_keywords(l)%movie_id) then
                      if (first) then
                        best_title = titles(k)%title
                        first = .false.
                      else
                        if (titles(k)%title < best_title) best_title = titles(k)%title
                      end if
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

  print '(A)', '"'//trim(best_title)//'"'
end program q2
