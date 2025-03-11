README
==============
The soil zone flowchart graphically describes the steps taken to calculate various reservoirs in table 108 of the PRMS IV manual.


Horizontal arrows
#----------
Horizontal arrows are calculations or movement of values that occur in the same step. For example reservoir A may have excess above a threshold removed and moved to reservoir B in the same computation step.


Vertical arrows
#----------
Arrows with any vertical component to them are values being passed between computation steps. For example the value of reservoir A is calculated in step 1 and its excess is moved to reservoir B. Reservoir C in step 2 is calculated and has the value of reservoir B added to it. In this case there would be a horizontal arrow between A and B, and a vertical arrow between B and C


Dotted lines
#----------
Dotted lines between elements of the flowchart should be interpreted as 'this entity could be either value A OR value B'. For example the gravity reservoir value carried into future calculations could either be that calculated in step 5 OR the value calculated in step 6 depending on the value of soil2gw_max. No calculations occur along dotted lines.


Solid lines
#----------
Solid lines represent the movement of values between either distinct entities or the same entity along multiple steps. For example the groundwater reservoir is calculated in step 6. An arrow then connects it to the groundwater reservoir in step 10. Calculations are not occurring along steps 7 through 9, the connection rather represents that whatever value calculated in step 6 is being passed to and used in step 10.