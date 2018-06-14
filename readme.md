# my socialist planning sim

I am building up a crude model of socialist economy

I am using Scheme because I just finished _The Structure and Interpretation
of Computer Programs_ and I want to keep trying it out. 

I am also reading Alec Nove's _The Economics of Feasible Socialism_. Although I am not ready to try to test out central planning versus market socialism, in the intial model I'm building the production units do send orders
to other production units in a decentralized manner. 

## simtools

simtools provides a set of tools to build and measure a simulated economy

* PRODUCERS take orders for their designated product, order the necessary products to make it, and then distribute the products produced. Producers have three components:
  
  * a stock of the product
  
  * a list of required products and the quantity needed of each to produce one new unit of stock

  * an inventory of products ordered

In the planning phase of the simulation, each producer calculates the shortfall between the amount of product it has on stock, and the amount of product ordered. 

It then estimates the quantities of other products it will need to make the shortfall, and sends orders to the other producers for the required amount

In initial simulations, economies would produce for a short while, and then production would stop. I believe there was not enough slack in the economy. I was able to solve this problem and ensure continuous produciton by simply adding 1 to the shortfall

* PLAN-DRIVERS generate orders to send to producer units

  * plan drivers have a current target that starts at 1

  * when a plan driver receives a shipment from a completed order, the plan driver is incremented by 1 until it reaches the final TARGET

NOTE: A plan driver is needed to kickstart an economy and keep it moving. The orders it produces stimulates orders to other production units. 

* an ECONOMY is a procedure that stores list of producers and plan drivers in its local environment. Producers and plan drivers use the economy to distribute orders and shipments to other production units

## measuring production

I'm using two measures of production:
* the current-target is the current-target the first plan-driver
in an economy is set to. The plan driver increments the current-target by 1
each time a previous target is met

* the plan-stride measures the number of time steps between the reaching of
two plan targets. An increasing plan stride means that an economy is 
slowing down. 
