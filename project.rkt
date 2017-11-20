#lang racket

(require net/url)

(define get-tweets
  (let ([fix (let ([clean-front (lambda (lst)
                                  (drop lst (add1 (index-of lst #\>))))]
                   [clean-rear (lambda (lst)
                                 (let ([i (index-of lst #\<)])
                                   (if i
                                       (take lst (index-of lst #\<))
                                       lst)))])
               (lambda (lst)
                 (let ([i (index-of lst #\<)])
                   (if i
                       (clean-rear (clean-front (drop lst (index-of lst #\<))))
                       lst))))]
        [good? (lambda (lst)
                 (let ([i (index-of lst #\<)])
                   (and i (string-contains? (list->string lst) "TweetTextSize--normal"))))])
    (lambda (link)
      (let* ([str (bytes->string/utf-8
                   (port->bytes
                    (get-pure-port (string->url link))))]
             [lst (string->list str)])
        (let kernel ([so-far null]
                     [remaining lst])
          (let ([i (index-of remaining #\newline)])
            (if i
                (let ([line (take remaining i)])
                  (if (good? line)
                      (kernel (append so-far (list (list->string (fix line))))
                              (drop remaining (add1 i)))
                      (kernel so-far
                              (drop remaining (add1 i)))))
                (if (good? remaining)
                    (append so-far (list (list->string (fix remaining))))
                    so-far))))))))

; Output:

;> (get-tweets "https://twitter.com/realDonaldTrump?ref_src=twsrc%5Egoogle%7Ctwcamp%5Eserp%7Ctwgr%5Eauthorz")
;'("Republican Senators are working very hard to get Tax Cuts and Tax Reform approved. Hopefully it will not be long and they do not want to disappoint the American public!"
;  "Border Patrol Officer killed at Southern Border, another badly hurt. We will seek out and bring to justice those responsible. We will, and must, build the Wall!"
;  "Big-game trophy decision will be announced next week but will be very hard pressed to change my mind that this horror show in any way helps conservation of Elephants or any other animal."
;  "Shoplifting is a very big deal in China, as it should be (5-10 years in jail), but not to father LaVar. Should have gotten his son out during my next trip to China instead. China told them why they were released. Very ungrateful!"
;  "Sen. Jeff Flake(y), who is unelectable in the Great State of Arizona (quit race, anemic polls) was caught (purposely) on “mike” saying bad things about your favorite President. He’ll be a NO on tax cuts because his political career anyway is “toast.”"
;  "Now that the three basketball players are out of China and saved from years in jail, LaVar Ball, the father of LiAngelo, is unaccepting of what I did for his son and that shoplifting is no big deal. I should have left them in jail!"
;  "Thank you "
;  "BOOM! Thank you, Mr President. Trophy-hunting is repellent."
;  "Crooked Hillary Clinton is the worst (and biggest) loser of all time. She just can’t stop, which is so good for the Republican Party. Hillary, get on with your life and give it another try in three years!"
;  "Put big game trophy decision on hold until such time as I review all conservation facts. Under study for years. Will update soon with Secretary Zinke. Thank you!"
;  "Today, it was an honor to celebrate the Collegiate National Champions of 2016/2017 at the "
;  "Together, we&#39;re going to restore safety to our streets and peace to our communities, and we&#39;re going to destroy the vile criminal cartel, "
;  "If Democrats were not such obstructionists and understood the power of lower taxes, we would be able to get many of their ideas into Bill!"
;  "Great numbers on Stocks and the Economy. If we get Tax Cuts and Reform, we&#39;ll really see some great results!"
;  ".And to think that just last week he was lecturing anyone who would listen about sexual harassment and respect for women. Lesley Stahl tape?"
;  "The Al Frankenstien picture is really bad, speaks a thousand words. Where do his hands go in pictures 2, 3, 4, 5 &amp; 6 while she sleeps? ....."
;  "Big win today in the House for GOP Tax Cuts and Reform, 227-205. Zero Dems, they want to raise taxes much higher, but not for our military!"
;  "Congratulations to the House of Representatives for passing the "
;  "Need all on the UN Security Council to vote to renew the Joint Investigative Mechanism for Syria to ensure that Assad Regime does not commit mass murder with chemical weapons ever again."
;  "China is sending an Envoy and Delegation to North Korea - A big move, we&#39;ll see what happens!")