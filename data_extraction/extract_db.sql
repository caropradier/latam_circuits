-- Extract data.

-- affiliation fix

SELECT p.pub_id, p.author_seq, MIN(p.affiliation_seq) AS affiliation_seq
  into #aux
  FROM [dimensions_2023jul].[dbo].[pub_author_affiliation] as p
  left join dimensions_2023jul.dbo.[pub_affiliation_country] as a on (p.pub_ID = a.pub_id and p.affiliation_seq = a.affiliation_seq)
  GROUP BY p.pub_id, p.author_seq

  SELECT p.pub_id, p.author_seq, p.affiliation_seq, a.affiliation_country_seq, a.country_code
  into #paper_affiliation_table
  FROM #aux as p
  left join dimensions_2023jul.dbo.[pub_affiliation_country] as a on (p.pub_ID = a.pub_id and p.affiliation_seq = a.affiliation_seq)

drop table #aux


-- find all Pub_id
--drop table #LA_id

select distinct P.Pub_ID
into #LA_id
from dimensions_2023jul.dbo.pub as p
left join #paper_affiliation_table as a on p.pub_ID = a.pub_id
where (p.pub_type_id = 1 OR p.pub_type_id = 6) and 

	  a.[country_code] in ('AR','BO','BR','CL','CO','MX','PY','PE','UY','VE') and
	  p.pub_year >=1990

--drop table #paper_affiliation_table

-- proceedings ids

select distinct P.Pub_ID
into #LA_proc_id
from dimensions_2023jul.dbo.pub as p
left join #paper_affiliation_table as a on p.pub_ID = a.pub_id
where  p.pub_type_id = 6 and 

	  a.[country_code] in ('AR','BO','BR','CL','CO','MX','PY','PE','UY','VE') and
	  p.pub_year >=1990


-- proceedings location

select distinct P.Pub_ID, p.conference_id,p.conference_location_id,cl.conference_location
into #LA_proc
from #LA_proc_id
left join dimensions_2023jul.dbo.pub as p on #LA_proc_id.pub_id=p.pub_id
left join [dimensions_2023jul].[dbo].[conference_location] as cl on p.conference_location_id=cl.conference_location_id
left join dimensions_2023jul.[dbo].pub_category as d on p.pub_id = d.pub_id
left join dimensions_2023jul.[dbo].[category] as cc on d.category_id = cc.category_id
WHERE cc.category_system_code = 'for'


-- article's metadata
--drop table #LA_art

select distinct P.Pub_ID, p.pub_year,sc.source_title, sct.source_type,Level1,level2, p.n_cits
into #LA_art
from #LA_id
left join dimensions_2023jul.dbo.pub as p on #LA_id.pub_id=p.pub_id
left join dimensions_2023jul.dbo.[pub_affiliation_country] as a on p.pub_ID = a.pub_id
left join dimensions_2023jul.[dbo].pub_category as d on p.pub_id = d.pub_id
left join dimensions_2023jul.[dbo].[category] as cc on d.category_id = cc.category_id
left join [userdb_larivierevg].dbo.conversion_table as ct on cc.category = ct.field
left join dimensions_2023jul.dbo.source as sc on P.source_id=sc.source_id
left join dimensions_2023jul.dbo.source_type as sct on sc.source_type_id=sct.source_type_id
WHERE cc.category_system_code = 'for'

-- abstracts and titles
--drop table #LA_abstracts

select distinct p.Pub_ID, t.title,ab.abstract
into #LA_abstracts
FROM #LA_id as p
inner join dimensions_2023jul.dbo.pub_title as t on p.pub_id = t.pub_id
left join dimensions_2023jul.dbo.pub_abstract as ab on p.pub_id = ab.pub_id

-- authors
--drop table #LA_authors
--drop #simplified_pub_author_affiliation

--FIX: we keep only the first affiliation of each author
select pub_id, author_seq, MIN(affiliation_seq) as affiliation_seq
into #simplified_pub_author_affiliation
from dimensions_2023jul.dbo.pub_author_affiliation
GROUP BY pub_id, author_seq


select distinct p.Pub_ID,aff.author_seq, pac.country_code,city.city,aut.author_id, aut.first_name,aut.last_name, gender
into #LA_authors
FROM #LA_id as p
inner join [dimensions_2023jul].dbo.pub_author as au on p.pub_id=au.pub_id
inner join dimensions_2023jul.dbo.author as aut on au.author_id=aut.author_id
inner join #simplified_pub_author_affiliation as aff on p.pub_id = aff.pub_id and au.author_seq=aff.author_seq
inner join dimensions_2023jul.dbo.pub_affiliation_country as pac on pac.pub_id=p.pub_id  and pac.affiliation_seq=aff.affiliation_seq
left join dimensions_2023jul.dbo.pub_affiliation_city as paci on paci.pub_id=p.pub_id  and paci.affiliation_seq=aff.affiliation_seq
left join dimensions_2023jul.dbo.city as city on city.geonames_city_id=paci.geonames_city_id
left join [userdb_larivierevg].[dbo].[authors_gender_sep2023] as gender on gender.author_id = au.author_id

-- DOI and ISSN

select distinct p.pub_id, pub.doi, source.issn_print --source.issn_e,
into #ids_merge
FROM #LA_id as p
inner join [dimensions_2023jul].dbo.pub_detail as pd on p.pub_id=pd.pub_id
inner join [dimensions_2023jul].dbo.pub as pub on pub.pub_id=p.pub_id
inner join [dimensions_2022jun].dbo.source as source on source.source_id=pub.source_id

--citation network info

select distinct p.citing_pub_id, p.cited_pub_id, p.cit_window, p.is_self_cit
into #LA_references
from #LA_id
left join dimensions_2023jul.dbo.citation as p on #LA_id.pub_id=p.citing_pub_id

select distinct p.citing_pub_id, p.cited_pub_id, p.cit_window, p.is_self_cit
into #LA_citations
from #LA_id
left join dimensions_2023jul.dbo.citation as p on #LA_id.pub_id=p.cited_pub_id



-- export

select * from #LA_art
--save to latam_meta_1990.csv

select * from #LA_abstracts
-- save to latam_text_1990.csv

select * from #LA_authors
-- save to latam_authors_1990.csv

select * from #ids_merge
-- save to latam_ids_1990.csv

select * from #LA_references
-- save to latam_references_1990.csv

select * from #LA_citations
-- save to latam_citations_1990.csv

select * from #LA_proc
--save to latam_conference_loc_1990.csv