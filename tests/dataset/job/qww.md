# JOB Query ww – Top US Movies After 2010

[qww.mochi](./qww.mochi) provides a tiny dataset with movie, company and rating information. It looks for movies produced by U.S. companies after 2010 with a rating above `8.0`. The query returns the highest rating and the alphabetically first matching title.

## SQL
```sql
SELECT MAX(mi.info) AS rating,
       MIN(t.title) AS us_movie
FROM company_name AS cn,
     company_type AS ct,
     movie_companies AS mc,
     title AS t,
     info_type AS it,
     movie_info_idx AS mi
WHERE cn.country_code = '[us]'
  AND ct.kind = 'production companies'
  AND it.info = 'rating'
  AND mi.info > 8.0
  AND t.production_year > 2010
  AND mc.company_id = cn.id
  AND ct.id = mc.company_type_id
  AND t.id = mc.movie_id
  AND mi.movie_id = t.id
  AND it.id = mi.info_type_id;
```

## Expected Output
Only one movie satisfies all conditions in the sample data:
```json
[{"rating": 8.9, "us_movie": "Great Movie"}]
```
