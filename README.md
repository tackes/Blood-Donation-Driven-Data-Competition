# Blood-Donation-Driven-Data-Competition
Author: Jeff Tackes

Project: Predict 454 Advanced Modeling

Northwestern University

Feburary 2018

Predicting Blood Donations - DrivenData Competition

RANK 69 / 3874 as of Febraruy 2018

https://www.drivendata.org/competitions/2/warm-up-predict-blood-donations/

The UCI Machine Learning Repository is a great resource for practicing your data science skills. They provide a wide range of datasets for testing machine learning algorithms. Finding a subject matter you're interested in can be a great way to test yourself on real-world data problems. Given our mission, we're interested in predicting if a blood donor will donate within a given time window.

Here's what the first few rows of the training set look like:

Months since Last Donation	Number of Donations	Total Volume Donated (c.c.)	Months since First Donation	Made Donation in March 2007
619	2	50	12500	98	1
664	0	13	3250	28	1
441	1	16	4000	35	1
160	2	20	5000	45	1
358	1	24	6000	77	0
Predict if the donor will give in March 2007
The goal is to predict the last column, whether he/she donated blood in March 2007.

Use information about each donor's history
Months since Last Donation: this is the number of monthis since this donor's most recent donation.
Number of Donations: this is the total number of donations that the donor has made.
Total Volume Donated: this is the total amound of blood that the donor has donated in cubuc centimeters.
Months since First Donation: this is the number of months since the donor's first donation.
Submission format
This competitions uses log loss as its evaluation metric, so the predictions you submit are the probability that a donor made a donation in March 2007.

The submission format is a csv with the following columns:

Made Donation in March 2007
659	0.5
276	0.5
263	0.5
303	0.5
83	0.5
To be explicit, you need to submit a file like the following with predictions for every ID in the Test Set we provide:

,Made Donation in March 2007
659,0.5
276,0.5
263,0.5
303,0.5
...
Data citation
Data is courtesy of Yeh, I-Cheng via the UCI Machine Learning repository:

Yeh, I-Cheng, Yang, King-Jang, and Ting, Tao-Ming, "Knowledge discovery on RFM model using Bernoulli sequence, "Expert Systems with Applications, 2008, doi:10.1016/j.eswa.2008.07.018.
