;;; -*- Mode: Common-lisp; Package: Guide; Readtable: Joshua -*-

(defun parse (string) (start:parse-text string :server "asist" :kb "yes" :format :xml))


(defparameter *sentence-1a* (parse "Person1 stands on a freeze tile accidentally"))
(defparameter *sentence-2* (parse "Person1 calls out for help.  I'm frozen.  Please come and unfreeze me."))
(defparameter *sentence-3* (parse "Person2 arrives and unfreezes Person1 who continues working."))

(defrule handle-standing-on (:backward)
  then [is-appropriate-response |stand| ?main handle-standing-on-free-tile (?person ?thing)]
  if [and [subject-of ?main ? ?person]
          [object-of ?main ? |null|]
          [as-subject ?main ?standing-on]
          [relation-of ?standing-on ? |on|]
          [object-of ?standing-on ? ?thing]
          [as-subject ?main ?tense-relation]
          [relation-of ?tense-relation ? |has_tense|]
          [object-of ?tense-relation ? |present|]]
  )


;;; A tester for this
(defmethod  get-handler-main-and-args ((parser core-parser-mixin) continuation)
  (get-mains-and-verb parser
                      #'(lambda (verb texp)
                          (ask* `[is-appropriate-response ,verb ,texp ?handler ?args]
                                (funcall continuation ?handler texp ?args))))
  (values))

#|
#1 Someone lands on a freeze tile and needs help from someone holding a MEDKIT to unfreeze them.


 Person2 arrives and unfreezes Person1 who continues working. 

 
Person2, who holds a MEDKIT responds that she is on the way.
[Or from player point of view, any of:
I am on the way.
I'm coming.
Here I come.]


[Alternative: Person2 arrives and unfreezes Person1 who continues what he was doing.
  PLEASE NOTE: If both Person1 and Person2 can be masculine, then this sentence is ambiguous and START defaultly assumes that "he" refers to "Person2" (on the grounds that Person1 is the _subject_ of "arrive" and "unfreeze", Person2 is the _object_ of "unfreeze" and we are looking for the _subject_ of "do").]

#2 Someone discovers a critically injured victim and needs two other people who can help save the victim.

Person1 is searching for victims when she discovers a critical victim.

Person1 announces that she has discovered a critical victim and that she needs the help of two others in order to provide care.
[Or from player PoV:
I've discovered a critical victim and I need the help of two others in order to provide care.]

Person 2 announces that she are on the way.

Person 3 announces that his is on the way.

Later, all three people are next to the critical victim and one person, who is holding a MEDKIT, begins to provide care.

#3 Person1, who holds a hammer, sees a collection of rubble which might hide a victim.

Person1 sees a pile of rubble.
Person1 sees that it is possible that a victim is hidden in the pile.
Person1 sufficiently clears rubble to see if there is a victim.
If a victim is found, Person1 announces the presence of the victim.

Possible ending: 3a.  Person2, who carries a stretcher, announces that he will bring out the victim to the corridor to facilitate easier care to be provided by Person3, who holds a MEDKIT.
 
Possible ending: 3b.  Person2 who caries a MEDKIT reports that she will be there soon to provide care.
[Alternative: Person2 who caries a MEDKIT reports that she will be there soon to provide care.]

#4 Person1, whose tool has expired, announces that he is going back to get a new tool.

While Person1 uses his tool, Person1 discovers that his tool has expired.
[Alternatives:
While Person1 uses his hammer, Person1 discovers that his hammer has expired.
etc.]

Person1 announces that he is going back to the tool room to get a new tool.
Perhaps with the help of the others, Person1 decides what he should come back with.
Person1 returns to the tool room.
Person1 gets a tool.
Person1 returns to the building to continue ongoing clearing of the building.
|#
