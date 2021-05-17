 (ENACT-STORY 'RESCUE-FROZEN-USER)


 Person1 is moving around in the building and accidentally stands on a freeze tile
Handling move around Person1 building yes present
Handling standing on freeze tile Person1 freeze_tile present
#<OBJECT (freeze_tile)> 

 Person1 calls out for help.  I'm frozen.  Please come and unfreeze me.
Handling Person1's call for help
Handling I's statement that he is frozen
Handling #<OBJECT (Person1)>'s request for someone to come in state #<state AFTER-STATEMENT>
Handling #<OBJECT (Person1)>'s request for someone to unfreeze it in state #<state AFTER-REQUEST-COME>

 Person2 has the medkit. Person2 arrives. Person2 unfreezes Person1.  Person1 resumes working.
Handling the fact that #<OBJECT (Person2)> has a #<OBJECT (medkit)> in state #<state AFTER-REQUEST-UNFREEZE>
Handling Person2 arrives at () #<OBJECT (freeze_tile)> #<OBJECT (freeze_tile)>

(Story:  RESCUE-FROZEN-worker
         (Actors: Person2 Person1)
         (Tools: medkit)
         (obstacles: freeze_tile)
         (vacinities: building)
         (Actions: MOVE-AROUND STEP-ON CALL-FOR-HELP STATES-OWN-CONDITION REQUEST-SOMEBODY-TO-COME REQUEST-SOMEBODY-TO-UNFREEZE ARRI
                   VE UNFREEZE WORKS)

         (Initial-state: 
          [IS-IN-VACINITY #<OBJECT (Person1)> #<OBJECT (building)>]
          [IS-IN-CONDITION #<OBJECT (Person1)> MOBILE]
          )

         (Action MOVE-AROUND: 
                 Next-State AFTER-MOVE-AROUND 
                 Assertions:
                 (IS-MOVING-AROUND Person1 building)
                 (NOT (IS-STANDING-ON Person1 freeze_tile)))

         (Action STEP-ON: 
                 Next-State AFTER-STEP-ON 
                 Assertions:
                 (IS-IN-VACINITY Person1 freeze_tile)
                 (IS-IN-CONDITION Person1 NEEDING-HELP)
                 (IS-STANDING-ON Person1 freeze_tile)
                 (NOT (IS-IN-CONDITION Person1 MOBILE))
                 (NOT (IS-WORKING Person1)))

         (Action CALL-FOR-HELP: 
                 Next-State AFTER-CALL-FOR-HELP 
                 Assertions:
                 (IS-CALLING-FOR Person1 HELP))

         (Action STATES-OWN-CONDITION: 
                 Next-State AFTER-STATEMENT 
                 Assertions:
                 (STATES-CONDITION Person1 NEEDING-HELP))

         (Action REQUEST-SOMEBODY-TO-COME: 
                 Next-State AFTER-REQUEST-COME 
                 Assertions:
                 (REQUESTS-SOMEONE-TO-COME Person1 ANYBODY))

         (Action REQUEST-SOMEBODY-TO-UNFREEZE: 
                 Next-State AFTER-REQUEST-UNFREEZE 
                 Assertions:
                 (IS-IN-POSSESSION-OF Person2 medkit)
                 (REQUESTS-SOMEONE-TO-UNFREEZE Person1 ANYBODY)
                 (IS-IN-CONDITION Person2 MOBILE))

         (Action ARRIVE: 
                 Next-State AFTER-ARRIVE 
                 Assertions:
                 (IS-IN-VACINITY Person2 freeze_tile))

         (Action UNFREEZE: 
                 Next-State AFTER-UNFREEZE 
                 Assertions:
                 (IS-IN-CONDITION Person1 MOBILE))

         (Action WORKS: 
                 Next-State AFTER-RESUMES-WORKING 
                 Assertions:
                 (IS-IN-CONDITION Person1 WORKING))
         )