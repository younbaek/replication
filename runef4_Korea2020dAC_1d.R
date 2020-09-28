# eforensics model applied to various datasets

library(eforensics);

# Korea 2020

dat <- read.csv("Korea2020dAC.csv");
names(dat);
dim(dat);
#> names(dat);
#  [1] "uid"         "name"        "area"        "sum_people"  "sum_vote"   
#  [6] "sum_invalid" "sum_novote"  "type"        "p3"          "p5"         
# [11] "p7"          "p9"          "p11"         "p113"        "p178"       
# [16] "p303"        "p401"        "p619"        "p621"        "p730"       
# [21] "p1247"       "p1312"       "p1363"       "p1519"       "p2433"      
# [26] "p2435"       "p2524"       "p2526"       "p2602"       "p2604"      
# [31] "p2822"       "p3341"       "p3442"       "p3743"       "p3809"      
# [36] "p3878"       "p3963"       "p4136"       "p4138"       "p4217"      
# [41] "p4388"       "p4572"       "p4784"       "p4786"       "p4972"      
# [46] "p5371"       "p5373"       "p5489"       "p5491"       "p5551"      
# [51] "p5553"       "p5629"       "p5631"       "p5686"       "p5688"      
# [56] "p5738"       "p5778"       "p5829"       "p5870"       "p5916"      
# [61] "p6351"       "p6405"       "p6460"       "p6527"       "p6668"      
# [66] "p6814"       "p6913"       "p7017"       "p7019"       "p7171"      
# [71] "p7231"       "p7351"       "p7476"       "p7520"       "p7565"      
# [76] "p7719"       "p7989"       "p7991"       "p8097"       "p8160"      
# [81] "p8277"       "p8279"       "p8407"       "p8409"       "p8468"      
# [86] "p8514"       "p8543"       "p8635"       "p8685"       "p8765"      
# [91] "p8821"       "p8841"       "p9254"       "p9355"       "p9443"      
# [96] "p9445"       "p9520"       "p9874"       "p10171"      "p10668"     
#[101] "p11269"      "p12800"      "p12947"      "p13223"      "p13323"     
#[106] "p13325"      "p13577"      "p13673"      "p13930"      "p13932"     
#[111] "p14132"      "p14436"      "p14485"      "p14555"      "p14616"     
#[116] "p14668"      "p14713"      "p14760"      "p14962"      "p14964"     
#[121] "p15050"      "p15129"      "p15697"      "p15895"      "p16112"     
#[126] "p16369"      "p16990"      "p16992"      "p16994"      "p17209"     
#[131] "p17358"      "p17440"      "p17502"      "p17504"      "p17590"     
#[136] "p17592"      "p17639"      "p17641"      "p17694"      "p17726"     
#[141] "p17793"      "p17824"      "p17959"      "p18037"      "p18039"     
#[146] "p18225"      "p18227"      "p18465"      "p18514"      "p18596"     
#[151] "p18659"      "p18860"      "p18918"      "p18988"      "p19038"     
#[156] "p19214"      "p19347"      "p19349"      "p19393"      "p19395"     
#[161] "p19485"      "p19487"      "p19532"      "p19639"      "p20112"     
#[166] "p20114"      "p20318"      "p20380"      "p20591"      "p20716"     
#[171] "p20718"      "p20781"      "p20921"      "p21009"      "p21156"     
#[176] "p21305"      "p21373"      "p21707"      "p21775"      "p22045"     
#[181] "p22191"      "p22687"      "p22844"      "p23085"      "p23409"     
#[186] "p23411"      "p23414"      "p23416"      "p23419"      "p23705"     
#[191] "p23771"      "p24032"      "p24034"      "p24113"      "p24209"     
#[196] "district"    "province"    "NVoters"     "NValid"      "Votes"      
#[201] "NAbst"       "isprevote"   "isabroad"    "isdisab"     "constit"    
#> dim(dat);
#[1] 19131   205

kidx <- !is.na(dat$NVoters) & (dat$NVoters >= dat$NValid) &
  (dat$NValid > 0) & (dat$NValid >= dat$Votes);
if (any(is.na(kidx))) kidx[is.na(kidx)] <- FALSE;
table(kidx);
vnames <- c("uid","name","sum_people",
  "sum_vote","sum_invalid","sum_novote","type","district","NVoters","NValid","Votes");
dat[!kidx,vnames];
tab <- table(dat[!kidx,"name"]);
tab[tab>0];
dat <- dat[kidx,];
dim(dat);

dat$NAbst <- dat$NVoters-dat$NValid;

#ef_byplot(dat, x="NValid/NVoters", y="Votes/NVoters",
# xlab="Turnout Prop. Among Eligible Voters", ylab="Prop. of Leading Votes Among Eligible Voters");
#table(dat$type=="d");
#ef_byplot(dat[dat$type=="d",], x="NValid/NVoters", y="Votes/NVoters",
# xlab="Turnout Prop. Among Eligible Voters", ylab="Prop. of Leading Votes Among Eligible Voters");
#ef_byplot(dat[dat$type=="p",], x="NValid/NVoters", y="Votes/NVoters",
# xlab="Turnout Prop. Among Eligible Voters", ylab="Prop. of Leading Votes Among Eligible Voters");

dat$isprevote <- dat$name=="prevote_out" | dat$name=="prevote_in";
table(dat$isprevote);
table(dat$isabroad <- dat$name=="abroad");
table(dat$isdisab <- dat$name=="disabled_ship");

dat$district <- factor(dat$district);
dat$constit <- factor(dat$constit);

## mcmc parameters
## ---------------
mcmc    = list(burn.in=5000, n.adapt=1000, n.iter=1000, n.chains=4)

efout <- eforensics(
  Votes ~ 1 + isprevote + type + isabroad + isdisab + constit,
  NAbst ~ 1 + isprevote + type + isabroad + isdisab + constit,  data=dat,
  eligible.voters="NVoters",
  model="qbl",  mcmc=mcmc,
  parameters = "all",  parComp = TRUE,  autoConv = TRUE,  max.auto = 10,
  mcmc.conv.diagnostic = "MCMCSE",
  mcmc.conv.parameters = c("pi"),  mcmcse.conv.precision = .05,  mcmcse.combine = FALSE
)

save(efout,file="runef4_Korea2020dAC_1d.RData");

summary(efout);

#Look at the estimated fraud proportions for each observation
#attr(efout,"frauds")
