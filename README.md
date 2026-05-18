# discord-reviews-nlp
Built an end‑to‑end NLP pipeline in R to extract insights from Discord user reviews using topic modeling and sentiment analysis. Preprocessed text data, identified key discussion themes, and quantified user sentiment to support data‑driven understanding of community feedback.

Topics
1.Communication/support issues
2.Login/security
3.Sentiment reactions
4.Social/gaming interaction
5.Features/Nitro ecosystem
6.Bugs/updates/system issues

Top terms per topic
     Topic 1   Topic 2  Topic 3   Topic 4   Topic 5   Topic 6    
[1,] "app"     "app"    "app"     "discord" "account" "good"     
[2,] "message" "good"   "good"    "nitro"   "get"     "app"      
[3,] "send"    "friend" "can"     "use"     "can"     "nice"     
[4,] "fix"     "great"  "discord" "people"  "discord" "cool"     
[5,] "update"  "chat"   "mobile"  "app"     "try"     "megaphone"
[6,] "time"    "talk"   "orb"     "get"     "log"     "love"  

Topic Distribution
  Topic_1 Topic_2 Topic_3 Topic_4 Topic_5 Topic_6
1   0.007   0.966   0.007   0.007   0.007   0.007
2   0.022   0.674   0.239   0.022   0.022   0.022
3   0.927   0.005   0.005   0.005   0.053   0.005
4   0.629   0.004   0.004   0.355   0.004   0.004
5   0.002   0.397   0.002   0.496   0.002   0.101
6   0.015   0.924   0.015   0.015   0.015   0.015

Dominant Topic
   1    2    3    4    5    6 
 914 1114  629  386  727  696 

Sentiment Scores
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-5.7500 -0.1000  0.5000  0.3923  0.7500  9.3000 

