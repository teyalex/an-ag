-- Factiva queries

/* despite the file extension, this isn't SQL code! the queries are pasted into a Factiva search as plain text.
I just changed the file extension to SQL here so that the Boolean operators and numbers are displayed in colored
text, making it all easier to read. */


-- Repeating Faunalytics/Sentient study

	la=en and hl=climate and sn=PUBLICATION NAME and wc>250
		-- from here, I manually selected and downloaded the first 100 relevant articles

-- Original landscape analysis

	-- Query segments

		-- Language, major U.S. sources, date range, word count greater than 250 /
		la=en and rst=tmnbus and date after 20210801 and date before 20250731 and wc>250

		-- Exclusions
		not (update or Briefing or summary or letters or correction* or Daybook or "market talk" or "roundup" or "press release" or "results announcement" or shortage)

		-- Mention of climate
		and (ns=GCLIMT and ((climate /n2/ chang*) or (climate /n2/ crisis) or (climate /n2/ impact) or "global warming" or ((atleast2 climate or atleast2 warming or atleast2 environment*) or hlp=carbon (neutrality or capture or emissions) or atleast2 greenhouse$ or atleast3 decarboni*)))

		-- Mention of meat consumption or production
		and (((meat or dairy or livestock or cow or cows or cattle or beef or sheep or lamb or chicken or pork) near3 (consum* or eat* or methane or carbon or CO2 or emissions)) or ((meat near2 industr*) or (livestock near2 industr*)) or ("animal agriculture" or "factory farm*"))

		-- Mention of dietary change
		((((diet* or consum* or “eat less” or “eating less” or “eat more” or “eating more”) near7 (meat or livestock or beef or plant-based or veg* or flexitarian or seafood or pescatarian or fish)) or (atleast2 vegan* or atleast2 vegetarian*)) and (climate or sustainab* or environment*))

	-- Constructed queries

		-- Climate (only)
		la=en and rst=tmnbus and date after 20210801 and date before 20250731 and wc>250 not (update or Briefing or summary or letters or correction* or Daybook or "market talk" or "roundup" or "press release" or "results announcement" or shortage) and (ns=GCLIMT and ((climate /n2/ chang*) or (climate /n2/ crisis) or (climate /n2/ impact) or "global warming" or ((atleast2 climate or atleast2 warming or atleast2 environment*) or hlp=carbon (neutrality or capture or emissions) or atleast2 greenhouse$ or atleast3 decarboni*)))

		-- Climate + meat
		la=en and rst=tmnbus and date after 20210801 and date before 20250731 and wc>250 not (update or Briefing or summary or letters or correction* or Daybook or "market talk" or "roundup" or "press release" or "results announcement" or shortage) and (ns=GCLIMT and ((climate /n2/ chang*) or (climate /n2/ crisis) or (climate /n2/ impact) or "global warming" or ((atleast2 climate or atleast2 warming or atleast2 environment*) or hlp=carbon (neutrality or capture or emissions) or atleast2 greenhouse$ or atleast3 decarboni*))) and (((meat or dairy or livestock or cow or cows or cattle or beef or sheep or lamb or chicken or pork) near3 (consum* or eat* or methane or carbon or CO2 or emissions)) or ((meat near2 industr*) or (livestock near2 industr*)) or ("animal agriculture" or "factory farm*"))

		-- Climate + meat + dietary change
		la=en and rst=tmnbus and date after 20210801 and date before 20250731 and wc>250 not (update or Briefing or summary or letters or correction* or Daybook or "market talk" or "roundup" or "press release" or "results announcement" or shortage) and (ns=GCLIMT and ((climate /n2/ chang*) or (climate /n2/ crisis) or (climate /n2/ impact) or "global warming" or ((atleast2 climate or atleast2 warming or atleast2 environment*) or hlp=carbon (neutrality or capture or emissions) or atleast2 greenhouse$ or atleast3 decarboni*))) and (((meat or dairy or livestock or cow or cows or cattle or beef or sheep or lamb or chicken or pork) near3 (consum* or eat* or methane or carbon or CO2 or emissions)) or ((meat near2 industr*) or (livestock near2 industr*)) or ("animal agriculture" or "factory farm*")) and ((((diet* or consum* or “eat less” or “eating less” or “eat more” or “eating more”) near7 (meat or livestock or beef or plant-based or veg* or flexitarian or seafood or pescatarian or fish)) or (atleast2 vegan* or atleast2 vegetarian*)) and (climate or sustainab* or environment*))
