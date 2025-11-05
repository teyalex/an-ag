-- Factiva queries
	
	/* despite the file extension, this isn't SQL code. I just set the file extension to .sql so that the Boolean operators and numbers are displayed in colored text, making it all easier to read. to perform the searches, paste the "Full queries" below into the Factiva search engine as plain text. */

-- Original landscape analysis

	-- Full queries
		-- These are the three searches we entered into Factiva.

		-- Climate (only)
		la=en and date after 20220630 AND date before 20250701 and wc>250 and rst=USA and rst=(sfabc or sfamb or sfbbc or bsun or sfbar or wc70138 or sfbusin or sfcnbc or sfcnn or trib or sfdpst or sfdfp or tdjw or sfforb or sffoxn or sfmsnbc or mrkwc or mlwk or mijswb or nydn or nypo or sfnwswk or orse or sfpittpg or flss or slmo or msp or stpt or phx or atjc or atlcom or sfbglob or chsm or wcchsm or cinc or hfct or indy or sfnyt or gtny or tnns or sfwsj or sfusat or sfwp) not (update or Briefing or summary or letters or correction* or Daybook or "market talk" or "roundup" or "press release" or "results announcement" or shortage) and (ns=GCLIMT and ((climate /n2/ chang*) or (climate /n2/ crisis) or (climate /n2/ impact) or "global warming" or ((atleast2 climate or atleast2 warming or atleast2 environment*) or hlp=carbon (neutrality or capture or emissions) or atleast2 greenhouse$ or atleast3 decarboni*)))

		-- Climate + meat/animal agriculture
		la=en and date after 20220630 AND date before 20250701 and wc>250 and rst=USA and rst=(sfabc or sfamb or sfbbc or bsun or sfbar or wc70138 or sfbusin or sfcnbc or sfcnn or trib or sfdpst or sfdfp or tdjw or sfforb or sffoxn or sfmsnbc or mrkwc or mlwk or mijswb or nydn or nypo or sfnwswk or orse or sfpittpg or flss or slmo or msp or stpt or phx or atjc or atlcom or sfbglob or chsm or wcchsm or cinc or hfct or indy or sfnyt or gtny or tnns or sfwsj or sfusat or sfwp) not (update or Briefing or summary or letters or correction* or Daybook or "market talk" or "roundup" or "press release" or "results announcement" or shortage) and (ns=GCLIMT and ((climate /n2/ chang*) or (climate /n2/ crisis) or (climate /n2/ impact) or "global warming" or ((atleast2 climate or atleast2 warming or atleast2 environment*) or hlp=carbon (neutrality or capture or emissions) or atleast2 greenhouse$ or atleast3 decarboni*))) and (((meat or dairy or livestock or cow or cows or cattle or beef or sheep or lamb or chicken or hog* or pig* or pork) near3 (rais* or consum* or eat* or methane or carbon or CO2 or emissions)) or (atleast3 meat or atleast3 dairy or atleast3 livestock or atleast3 cow or atleast3 cows or atleast3 cattle or atleast3 beef or atleast3 sheep or atleast3 lamb or atleast3 chicken or atleast3 poultry or atleast3 hog or atleast3 hogs or atleast3 pig or atleast3 pigs or atleast3 pork) or ((meat near2 industr*) or (livestock near2 industr*)) or ("animal agriculture" or "factory farm*"))

		-- Climate + meat/animal agriculture + dietary change
		la=en and date after 20220630 AND date before 20250701 and wc>250 and rst=USA and rst=(sfabc or sfamb or sfbbc or bsun or sfbar or wc70138 or sfbusin or sfcnbc or sfcnn or trib or sfdpst or sfdfp or tdjw or sfforb or sffoxn or sfmsnbc or mrkwc or mlwk or mijswb or nydn or nypo or sfnwswk or orse or sfpittpg or flss or slmo or msp or stpt or phx or atjc or atlcom or sfbglob or chsm or wcchsm or cinc or hfct or indy or sfnyt or gtny or tnns or sfwsj or sfusat or sfwp) not (update or Briefing or summary or letters or correction* or Daybook or "market talk" or "roundup" or "press release" or "results announcement" or shortage) and (ns=GCLIMT and ((climate /n2/ chang*) or (climate /n2/ crisis) or (climate /n2/ impact) or "global warming" or ((atleast2 climate or atleast2 warming or atleast2 environment*) or hlp=carbon (neutrality or capture or emissions) or atleast2 greenhouse$ or atleast3 decarboni*))) and (((meat or dairy or livestock or cow or cows or cattle or beef or sheep or lamb or chicken or hog* or pig* or pork) near3 (rais* or consum* or eat* or methane or carbon or CO2 or emissions)) or (atleast3 meat or atleast3 dairy or atleast3 livestock or atleast3 cow or atleast3 cows or atleast3 cattle or atleast3 beef or atleast3 sheep or atleast3 lamb or atleast3 chicken or atleast3 poultry or atleast3 hog or atleast3 hogs or atleast3 pig or atleast3 pigs or atleast3 pork) or ((meat near2 industr*) or (livestock near2 industr*)) or ("animal agriculture" or "factory farm*")) and ((((diet* or consum* or “eat less” or “eating less” or “eat more” or “eating more”) near7 (meat or livestock or beef or plant-based or veg* or flexitarian or seafood or pescatarian or fish)) or (atleast2 vegan* or atleast2 vegetarian*)) and (climate or sustainab* or environment*))

	-- Query sections
		-- These segments were used to assemble the full queries.

		-- Language, date range, word count greater than 250, published in the United States
		la=en and date after 20220630 AND date before 20250701 and wc>250 and rst=USA

		-- Major U.S. sources
		and rst=(sfabc or sfamb or sfbbc or bsun or sfbar or wc70138 or sfbusin or sfcnbc or sfcnn or trib or sfdpst or sfdfp or tdjw or sfforb or sffoxn or sfmsnbc or mrkwc or mlwk or mijswb or nydn or nypo or sfnwswk or orse or sfpittpg or flss or slmo or msp or stpt or phx or atjc or atlcom or sfbglob or chsm or wcchsm or cinc or hfct or indy or sfnyt or gtny or tnns or sfwsj or sfusat or sfwp)

		-- Mention of climate
		and (ns=GCLIMT and ((climate /n2/ chang*) or (climate /n2/ crisis) or (climate /n2/ impact) or "global warming" or ((atleast2 climate or atleast2 warming or atleast2 environment*) or hlp=carbon (neutrality or capture or emissions) or atleast2 greenhouse$ or atleast3 decarboni*)))

		-- Exclusions
		not (update or Briefing or summary or letters or correction* or Daybook or "market talk" or "roundup" or "press release" or "results announcement" or shortage)

		-- Mention of meat consumption or production
		and (((meat or dairy or livestock or cow or cows or cattle or beef or sheep or lamb or chicken or pork) near3 (consum* or eat* or methane or carbon or CO2 or emissions)) or ((meat near2 industr*) or (livestock near2 industr*)) or ("animal agriculture" or "factory farm*"))

		-- Mention of dietary change
		((((diet* or consum* or “eat less” or “eating less” or “eat more” or “eating more”) near7 (meat or livestock or beef or plant-based or veg* or flexitarian or seafood or pescatarian or fish)) or (atleast2 vegan* or atleast2 vegetarian*)) and (climate or sustainab* or environment*))

-- Repeating Faunalytics/Sentient study

	la=en and hl=climate and sn=PUBLICATION NAME and wc>250
		-- from here, I manually selected and downloaded the first 100 relevant articles for each publication.
