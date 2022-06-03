
--Didn't work (brudd på unik skranke / unique constraint (string.string) violated )  
-- insert into NIVADATABASE.METHOD_DEFINITIONS (NAME, UNIT, LABORATORY, METHOD_REF, MATRIX, CAS, MATRIX_ID)
-- values ('Intersex', 'PERCENT', 'NIVA', NULL, 'Biota', NULL, 1);

--Didn't work (brudd på unik skranke / unique constraint (string.string) violated )  
-- insert into NIVADATABASE.METHOD_DEFINITIONS (NAME, UNIT, LABORATORY, MATRIX, MATRIX_ID)
-- values ('Intersex', 'PERCENT', 'NIVA', 'Biota', 1);

-- Worked
-- Seems that MATRIX_ID = 1 was entered anyway??
insert into NIVADATABASE.METHOD_DEFINITIONS (NAME, UNIT, LABORATORY)
values ('Intersex', 'PERCENT', 'NIVA');


-- For deleting:
--DELETE from NIVADATABASE.METHOD_DEFINITIONS where TRUNC(ENTERED_DATE) = TO_DATE('2021-09-10', 'yyyy-mm-dd') and ENTERED_BY = 'DHJ';
-- undo delete:
-- ROLLBACK;

