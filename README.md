# MoN3
Software developed for MoN3 Rome 2015/03/10-11

## "Usage"

Having a .gexf file from Edgesense, you need to convert the file to .graphml (can be done through Python Networkx).

If g is a graph,
   g=networkHealth(g)

Will return:
* g$respOutcome with columns modEvents and nonModEvents which show for each year and week the number of moderator-originated and non moderator-originated events
* g$nodeActivity as a data frame with columns nId, inPosts, outPosts and weeksSinceCreated (for the node)
* g$drm as a drc dose-response curve fitted model for nonModEvents vs modEvents
