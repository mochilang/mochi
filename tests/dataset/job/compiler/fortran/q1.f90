program q1
  implicit none
  type :: CompanyType
    integer :: id
    character(len=32) :: kind
  end type CompanyType
  type :: InfoType
    integer :: id
    character(len=32) :: info
  end type InfoType
  type :: TitleRec
    integer :: id
    character(len=32) :: title
    integer :: year
  end type TitleRec
  type :: MovieCompany
    integer :: movie_id
    integer :: company_type_id
    character(len=64) :: note
  end type MovieCompany
  type :: MovieInfoIdx
    integer :: movie_id
    integer :: info_type_id
  end type MovieInfoIdx
  type(CompanyType) :: company_types(2)
  type(InfoType) :: info_types(2)
  type(TitleRec) :: titles(2)
  type(MovieCompany) :: movie_companies(2)
  type(MovieInfoIdx) :: movie_info_idxs(2)
  character(len=64) :: min_note
  character(len=32) :: min_title
  integer :: min_year
  logical :: first
  integer :: i,j,k,l,m
  character(len=32) :: s_year
  character(len=256) :: out

  company_types(1) = CompanyType(1,'production companies')
  company_types(2) = CompanyType(2,'distributors')

  info_types(1) = InfoType(10,'top 250 rank')
  info_types(2) = InfoType(20,'bottom 10 rank')

  titles(1) = TitleRec(100,'Good Movie',1995)
  titles(2) = TitleRec(200,'Bad Movie',2000)

  movie_companies(1) = MovieCompany(100,1,'ACME (co-production)')
  movie_companies(2) = MovieCompany(200,1,'MGM (as Metro-Goldwyn-Mayer Pictures)')

  movie_info_idxs(1) = MovieInfoIdx(100,10)
  movie_info_idxs(2) = MovieInfoIdx(200,20)

  first = .true.
  do i=1,2
    if (trim(company_types(i)%kind) == 'production companies') then
      do j=1,2
        if (movie_companies(j)%company_type_id == company_types(i)%id) then
          if (index(movie_companies(j)%note,'(as Metro-Goldwyn-Mayer Pictures)') == 0) then
            if (index(movie_companies(j)%note,'(co-production)') > 0 .or. &
                index(movie_companies(j)%note,'(presents)') > 0) then
              do k=1,2
                if (titles(k)%id == movie_companies(j)%movie_id) then
                  do l=1,2
                    if (movie_info_idxs(l)%movie_id == titles(k)%id) then
                      do m=1,2
                        if (info_types(m)%id == movie_info_idxs(l)%info_type_id .and. &
                            trim(info_types(m)%info) == 'top 250 rank') then
                          if (first) then
                            min_note = movie_companies(j)%note
                            min_title = titles(k)%title
                            min_year = titles(k)%year
                            first = .false.
                          else
                            if (movie_companies(j)%note < min_note) min_note = movie_companies(j)%note
                            if (titles(k)%title < min_title) min_title = titles(k)%title
                            if (titles(k)%year < min_year) min_year = titles(k)%year
                          end if
                        end if
                      end do
                    end if
                  end do
                end if
              end do
            end if
          end if
        end if
      end do
    end if
  end do

  call fmt_int(min_year, s_year)
  out = '[{"movie_title":"'//trim(min_title)//'","movie_year":'//trim(s_year)//',"production_note":"'//trim(min_note)//'"}]'
  print '(A)', trim(out)
contains
  subroutine fmt_int(x, res)
    integer, intent(in) :: x
    character(len=*), intent(out) :: res
    write(res,'(I0)') x
    res = trim(adjustl(res))
  end subroutine
end program q1
