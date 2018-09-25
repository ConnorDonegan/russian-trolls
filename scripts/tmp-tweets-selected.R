




bots %>%
  filter(publish_date == as.Date("2017-08-12")) %>%
  select(author, content) %>%
  filter(str_detect(content, "nazi|white nationalist|unite the right")) %>%
  data.table::fwrite("data/select-bots/aug12.txt")


jenn_tweets <- bots %>%
  filter(author == "jenn_abrams" &
           abs(publish_date - as.Date("2016/11/06")) < 15) %>%
  select(publish_date, content) 


ten_tweets <- bots %>%
  filter(author == "ten_gop" &
           abs(publish_date - as.Date("2016/11/06")) < 15) %>%
  select(publish_date, content) 



## rigth troll word cloud
bots %>%
  filter(author == "ten_gop") %>%
  arrange(desc(publish_date)) %>%
  mutate(content = str_replace_all(content, "https://t.co/[A-Za-z\\d]+|&amp;|http", "")) %>%
  select(publish_date, content, retweet) %>%
  data.table::fwrite("data/select-bots/ten-gop.txt")
  
  
  unnest_tokens(output = word, input = content, token = 'words') %>%
  filter(!str_detect(word, web)) %>%
  anti_join(get_stopwords()) %>%
  count(word, sort = TRUE) %>%
  rename(freq = n) %>%
  wordcloud2()


bots %>%
  filter(author == "jenn_abrams" &
           publish_date > as.Date("2016/03/10")) %>%
  select(publish_date, content) %>%
  arrange(publish_date) %>%
  write.table("jenn-abrams2.txt",sep = "\t")


2015-09-18	"'@realdonaldtrump trump increases idiocy in american politics'"
2015-09-21	"oh, there are so many things in the world that look just like trump's hair "
2015-09-21	"curious fact: 1 out of 3 trump supporters are just as stupid as the other 2"
2015-09-22	"\"sexy #donaldtrump\" halloween costume for sale  let me unsee it

2015-09-26	"a 51-year-old german woman is being evicted from her home of 16 years to make way for refugees "
2015-09-28	"women? elderly? children? no, welfare seekers and probable terrorists #refugeescrisis #migrantcrisis "
2015-10-17	"tension builds between 2016 gop rivals rubio and bush. they both are worthless "
2015-11-08	"while your children are learning to tolerate these children, these children are learning to kill your children "
2015-12-01	"nearly 1,000 clinton emails had classified info  why is she still running?! "
2015-12-02	"multiculturalism - when a chechen, a french, a somali and a pakistani gather to cut throats of infidels "
2015-12-15	"trump supporters shout 'sieg heil' at a rally in vegas. our nation is way more f**ed than i thought "

2016-01-16	"who is a greater threat to women, the immigrant rapist, or the politician who let him in and won’t deport him?"
2016-02-27	"the president we deserve? #nevertrump "

2016-04-14	"some people people want to believe that #refugees suffer, but they enjoy their european life: stealing and raping "
2016-05-19	"this election season will cost more than 5 billion dollars. there must be a cheaper way to find the worst people in the society"
2016-06-16	" colored people are human weeds and need to be exterminated  hillary: i admire margaret sanger enormously <U+FFFD> "
2016-08-18	"new trump campaign chief's account isn't even @verified on twitter "
2016-11-12	"alleged pedophile and human trafficking ring leading to podestas is exposed. it's disturbing! #pizzagate more on  "

bots %>%
  filter(author == "politweecs") %>%
  select(publish_date, content) %>%
  arrange(publish_date) %>%
  write.table("politweecs.txt",sep = "\t")

2015-04-09	"dzhokhar #tsarnaev found guilty of bombing #bostonmarathon. oh, those russians"
2015-04-09	"a new campaign slogan for #hillaryclinton: never think"
2015-04-15	"the choice is either #clinton's wife or #bush's brother. have you ever heard about transfer of power? #hillaryclinton2016 #jebbush2016"
2015-04-15	"'@bbcworld our borders are open for everyone thanks to #immigrationreform'"
2015-04-19	"seems like we don't negotiate with #terrorists. we just apologize to them, or leave this part to the next generations"
2015-04-28	"#obama blames social ills for crisis in #baltimore. of course,  everyone is guilty except you #baltimoreriots "
2015-04-28	"donald trump is a great buisnessman but he has zero experience in government and government doesn't work like a business #election2016"
2015-06-03	"'@govmikehuckabee: wish i could've claimed to be #transgender to ' shower with the girls'. wish it was my idea, too! "
2015-06-05	"rick perry thinks we need a president who’s served in the military. to continue endless wars, of course"
2015-06-15	"a teen died after she went into cardiac arrest while getting her wisdom teeth pulled "
2015-06-21	"'@washingtonpost being gay is not a choice. being racist too'"
2015-06-30	"#donaldtrump  is a published author. he has written at least 50 books. his first book, the art of the deal. "
2015-07-02	"poll: majority sees #confederate flag as #southernpride symbol, not racist. in our country only minoritys' opinion is important"
2015-07-11	"'@realdonaldtrump @vplus yeah, yeah great again. how about to buy a new wig? you should try the black'"
2015-07-12	"donald trump @realdonaldtrump ‘we have to take back the heart of our country’ "
2015-07-17	"#trump campaign paying  a month in rent — to trump "
2015-07-31	"'@realdonaldtrump what would happen if you actually became president? i think funny apocalypse'"

2015-08-07	"#donaldtrump \"you would not even talk about it,until i brought it up\"(about immigrants). i have to admit, he is right "

2015-08-14	"police officer jumped onto a handcaffed teen after putting him in the police cruiser. stop #policebrutality already!"
2015-08-20	"#trump thinks there are a lot of good people among the immirants. but illegal immigrants have to leave. fair enough "


bots %>%
  filter(author == "worldofhashtags") %>%
  select(publish_date, content) %>%
  arrange(publish_date) %>%
  write.table("worldofhashtags.txt",sep = "\t")
2016-06-21	"we don't know exactly who's our future president,but obama needs a new job surely assume #obamanextjob and have fun! "
"8215"	2016-06-21	"proof reader #obamanextjob"
"8216"	2016-06-21	"fox news intern #obamanextjob"
"8217"	2016-06-21	"#obamanextjob ice-cream tester "
"8218"	2016-06-21	"obama impersonator #obamanextjob"
"8219"	2016-06-21	"#obamanextjob pussy eater"
"8220"	2016-06-21	"trump weave stylist #obamanextjob"
"8221"	2016-06-21	"supreme court justice  #obamanextjob"
"8222"	2016-06-21	"just for men model #obamanextjob"
"8223"	2016-06-21	"#obamanextjob  snake oil salesman.  oh, "
"8224"	2016-06-21	"#obamanextjob worst potus actor ever"
"8225"	2016-06-21	"#obamanextjob what ever he done before he was president.  being president doesn't disqualify you from that does it?"
"8226"	2016-06-21	"president of cuba #obamanextjob"
"8227"	2016-06-21	"#obamanextjob ice cream man"
"8228"	2016-06-21	"president of isis #obamanextjob #jk"
"8229"	2016-06-21	"#obamanextjob hanging curtains. "
"8230"	2016-06-21	"morgan freeman double #obamanextjob"
"8231"	2016-06-21	"stars as will smith's dad in independence day part 3 #obamanextjob"
"8232"	2016-06-21	"star in the next dwayne johnson buddy flick #obamanextjob"
"8233"	2016-06-21	"mayor of chicago #obamanextjob"
"8234"	2016-06-21	"speech writer for donald trump #obamanextjob"
"8235"	2016-06-21	"#obamanextjob before and after advertisement "
"8236"	2016-06-21	"president of the nra  #obamanextjob"
"8237"	2016-06-21	"#obamanextjob  supreme court.  (janitor)"
"8238"	2016-06-21	"#obamanextjob professional mic dropper "
"8239"	2016-06-21	"#obamanextjob advisor. or a pimp named slickback. "
"8240"	2016-06-21	"breaking and entering cause no one will hire him #obamanextjob "
...
2016-06-21	"#obamanextjob imam abu barack bin al obama al afriki"
"8262"	2016-06-21	"#obamanextjob trumps hairdresser"
"8263"	2016-06-21	"#obamanextjob being a career father."
"8264"	2016-06-21	"#obamanextjob become a professional basketball player "
2016-06-21	"#obamanextjob vladimir putin's k-y jelly warehouse foreman"


bots %>%
  filter(author == "worldofhashtags") %>%
  select(publish_date, content) %>% # also #growingupwithobama
  dplyr::filter(str_detect(content, "obamanextjob")) %>%
  summarise(n()) # 505 rows of this crap
2016-07-06	"#growingupwithobama is self confident muslims  all over the usa"
2016-07-06	"#growingupwithobama means that only black lives matter #blm #blacklivesmatter"
2016-07-06	"#growingupwithobama we were orphans together in africa before getting adopted by osama bin laden, at least according to trump"
2016-07-20	"#makeamovieblack black men in black "
2016-07-20	"#makeamovieblack black to the future"
2016-07-27	"#ifhillarybecomespresident she'll finally shave her legs."
2016-07-27	"#ifhillarybecomespresident she'll solve the healthcare needs of us veterans by deporting them."
2016-07-30	"#ihatepokemongobecause it's significantly reduced my couch potato time"
"#myolympicsportwouldbe sleeping on cue because i'm always tired"
2016-08-04	"islam is the #religionofpeace  #obamaswishlist"
2016-08-04	"#obamaswishlist a small loan of a million dollars from trump "
2016-08-04	"#obamaswishlist i wish trump would like me back"

2016-08-17	"putin reveals secret man crush for trump #trumpsfavoriteheadline"
2016-08-17	"#trumpsfavoriteheadline vladimir putin congratulated trump on his presidency"
2016-08-17	"#trumpsfavoriteheadline new plague killing only people of color"
 # lots of small hand jokes!
2016-08-17	"actually, i'm like10 percent sure he doesn't even know how to read. #trumpsfavoriteheadline"
2016-08-17	"#trumpsfavoriteheadline constitution is outdated, america becomes a monarchy"

2016-10-19	"which country should we illegally bomb and invade next? #rejecteddebatetopics"
2016-10-19	"if julian assange is lying. why is the mainstream media trying so hard to silence him? #rejecteddebatetopics"
"#sometimesitsokto have dessert first" # - shit in the pool
#after election day: #icelebratetrumpwith  mixed responses.
