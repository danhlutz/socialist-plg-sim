# my socialist planning sim

I am building up a crude model of socialist economy

I am using Scheme because I just finished _The Structure and Interpretation
of Computer Programs_ and I want to keep trying it out. 

I am also reading Alec Nove's _The Economics of Feasible Socialism_. Although I am not ready to try to test out central planning versus market socialism, in the intial model I'm building the production units do send orders
to other production units in a decentralized manner. 

## simtools

simtools provides a set of tools to build and measure a simulated economy

## measures of success

I'm using two measures of success:
* the current-target is the current-target the first plan-driver
in an economy is set to. The plan driver increments the current-target by 1
each time a previous target is met

* the plan-stride measures the number of time steps between the reaching of
two plan targets. An increasing plan stride means that an economy is 
slowing down
