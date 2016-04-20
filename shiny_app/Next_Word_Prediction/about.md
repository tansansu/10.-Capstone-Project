### About this web app
---
#### What is this?
This web app is the 'Capstone product' for Data science specilization of Coursera. This will predict a word that you are going to type.

#### How to use

1. Type any senteces or words in english.

2. Then, the words that you are likely to type will be shown below(maximum 3 words).

3. Enjoy! :-)

#### My prediction algorithm

++I used n-grams and the 'Back-Off model' for this work.++ My algorithm is below.

* I sampled the 20,000 sentences from each raw text files(twitter, news, blogs).
* I made the quad-gram, tri-gram, bi-gram and uni-gram from the above sampled data. And, I merged each n-grams into one data set.
* The data set has 1,655,508 rows and 3 columns. And, the shape of the table is below.

| Lookup | Recommend | Freq |
|:---:|:---:|---|
|the end of|the|72|
|one of|the|347|
|of|the|4165|
|...|...|...|

* If some words are inputted, the app searches matched n-gram in 'Lookup' column. After that, it is going to recommend a word in 'Recommend' column on the same row with Lookup' column of matched n-gram with inputted words.
* Next, those words are grouped by same words and computed the probability of the appearance. The formula I set is below.

```
[ { (0.5) x '4-gram' + (0.3) x '3-gram' + (0.2) x '2-gram' } x 0.95 ] + { '1-gram' x (0.05) }
```

* The 1-gram word in above formula is the recommended word by 4~2-gram through 'Back-Off model'. I added 0.05 weight to the 1-gram words to give a priority to words which people frequently used.

* If it fail to match with any n-grams, this app shows the most frequently used three words(1. the, 2. to, 3. and).


##### The [link](http://tansansu.github.io/shiny_app/index.html) of this web app. And, the source codes are in [here](https://github.com/tansansu/10.-Capstone-Project).



