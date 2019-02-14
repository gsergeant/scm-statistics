#specify location of datasets
#working directory?

coordinates <- read.table("coordinates.txt", header=TRUE, sep=",")
#originele data zijn enorm opgesplitst:
#1 tabel coordinaten per treatment (/4)
#voor elke patient (/3), en er zijn 3 ziektebeelden
# == 36 tabellen in totaal
#Nina has coded in python how to integrate these into one big table, with annotations for disease, patients and treatment


#load dataset in memory, format csv. Header?
#load what, tracks, features....?



#show dataset head & additional info
dim(coordinates)
head(coordinates)
summary(coordinates)

#set groups?

#make sure track number is unique??

#-----------------------------------------------------------------------------------
#volledige dataframe voor alles(lineair model, hoofdeffect voor treatment en ziekte, blokfactor voor patient)
#effect  van treatment binnen één patient kunnen we schatten --> efet van patient weghalen
#treatment effect schatten adhv within patient error , block design
#dsease effect within en between patient errors
#efect verschil t1 en t2 tussen ziektes zelfde?

#alle effecten schatten: mixed model
#controle treatment als baseline gebruiken voor verschillen tussen treatment effecten (t1-t2, tussen D1 en D2)

#praktisch: lineair model(data~treatment + patient)
#en gepaarde t test

#mds plot of pca exploratief doen

#residuelen analyse van linear gefit model. errors normaal verdeeld?
#resuduelen moeten liefst rond 0 want staan orthogonaal op de fit.
#en variantie van de residuelen moet gelijk blijven
#ln(data~DxT+P)
#lineair mixed model: lmer(data~DxT+(1|p))

#-------------------------------------------------------------------------------
# somehow calculate features: step and trajectory
list_  <-  list()
for cellType_treatment in unique(dataf[["column"]]):	#...in tracks.cellType_treatment.unique():
	print('current condition: {}'.format(cellType_treatment))
	tmp  <-  tracks[tracks.cellType_treatment==cellType_treatment]
	for track in unique(tmp[[track_id]]):			#...tmp.track_id.unique():

		tmp2  <-  tmp[tmp.track_id==track]
		array  <-  tmp2.drop(['track_id', 't', 'cellType_treatment'], axis=1).as_matrix()

		diff_array  <-  array - array[0]
		diff_df  <-  pd.DataFrame(diff_array, columns = ['norm_x', 'norm_y'])
		list_.append(diff_df)

tracks_norm <- pd.concat(list_)
tracks_norm <- tracks_norm.assign(track_id=tracks.track_id.values, t=tracks.t.values, x=tracks.x.values, y=tracks.y.values,
                                 cellType_treatment=tracks.cellType_treatment.values);
head(tracks_norm)





list_<- list()
for cellType_treatment in unique(tracks[[cellType_treatment]]):		#...tracks.cellType_treatment.unique():
  print('current condition: {}'.format(cellType_treatment))
tmp = tracks[tracks.cellType_treatment==cellType_treatment]
for track in unique(tmp[[track_id]]):

  tmp2 = tmp[tmp.track_id==track]

array = tmp2.drop(['track_id', 't', 'cellType_treatment'], axis=1).as_matrix()
array =np.array(array, dtype=np.float)

diff_array = np.diff(array, axis=0)

diff_array = np.insert(diff_array, [0], [np.NaN, np.NaN], axis=0)

diff_df = pd.DataFrame(diff_array, columns = ['delta_x', 'delta_y'])
list_.append(diff_df)

tracks_delta = pd.concat(list_)
tracks_delta = tracks_delta.assign(track_id=tracks.track_id.values, t=tracks.t.values, x=tracks.x.values, y=tracks.y.values,
                                   norm_x=tracks_norm.norm_x, norm_y=tracks_norm.norm_y, cellType_treatment=tracks.cellType_treatment.values);
head(tracks_delta)






list_ <- list()
for cellType_treatment in unique(tracks_delta[[cellType_treatment]]):	#...tracks_delta.cellType_treatment.unique():
  print('current condition: {}'.format(cellType_treatment))
tmp = tracks_delta[tracks_delta.cellType_treatment==cellType_treatment]
for track in unique(tmp[[track_id]]):		#...tmp.track_id.unique():

  tmp2 = tmp[tmp.track_id==track]
array = tmp2.drop(['track_id', 't', 'cellType_treatment', 'norm_x', 'norm_y', 'x', 'y'], axis=1).as_matrix()

t_a = np.apply_along_axis(lambda x: math.atan2(x[0], x[1]),1,array)
df = np.column_stack((array, t_a))
df = pd.DataFrame(df, columns = ['delta_x', 'delta_y', 'ta'])
list_.append(df)

tracks_ta = pd.concat(list_)
tracks_ta = tracks_ta.assign(track_id=tracks.track_id.values, t=tracks.t.values, x=tracks.x.values, y=tracks.y.values,
                             norm_x=tracks_norm.norm_x, norm_y=tracks_norm.norm_y, cellType_treatment=tracks.cellType_treatment.values);
head(tracks_ta)





def to_deg(angle):
  if angle == 'nan':
  return float('nan')
elif angle > 0:
  return math.degrees(angle)
else:
  return math.degrees(angle) + 360

tracks_ta['ta_deg'] = tracks_ta.ta.apply(to_deg)
head(tracks_ta)



list_ <- list()
for cellType_treatment in unique(tracks_delta[[cellType_treatment]]):	#...tracks_delta.cellType_treatment.unique():
  print('current condition: {}'.format(cellType_treatment))
tmp = tracks_delta[tracks_delta.cellType_treatment==cellType_treatment]
for track in tmp.track_id.unique():

  tmp2 = tmp[tmp.track_id==track]
array = tmp2.drop(['track_id', 't', 'cellType_treatment', 'norm_x', 'norm_y', 'x', 'y'], axis=1).as_matrix()

delta_z = np.apply_along_axis(lambda x: math.hypot(x[0], x[1]),1,array)
df = pd.DataFrame(delta_z, columns = ['delta_z'])
list_.append(df)

tracks_final = pd.concat(list_)
tracks_final = tracks_final.assign(delta_x=tracks_delta.delta_x, delta_y = tracks_delta.delta_y,
                                   track_id=tracks.track_id.values, t=tracks.t.values, x=tracks.x.values, y=tracks.y.values,
                                   norm_x=tracks_norm.norm_x, norm_y=tracks_norm.norm_y, cellType_treatment=tracks.cellType_treatment.values,
                                   ta_deg=tracks_ta.ta_deg.values);
head(tracks_final)


#### extra features like convex hull and euclidian distance, etc.
#problem: cellmissy code requires data to be in database =(
#FEATURES: Net x and y displacement start - finish; convex hull stuff; cumulative and euclidian distance; displacement ratio;
# outreach ratio; endpoint directionality ratio; median displacement, turning angle and median directionalty ratio...

#Compute the convex hull. THis returns the index for the X and Y coordinates
c.hull <- chull(data.tracks) #2-column matrix
#Extract the points from the convex hull. Note we are using the row indices again.
data.tracks[c.hull ,]
#for features we need acircularity, area, perimeter, directionality and maximum span between furthest points

#Make a pretty plot
with(data.tracks, plot(X,Y))
lines(data.tracks[c.hull ,], col = "pink", lwd = 3)
###Note if you wanted the bounding box
library(spatstat)
box <- bounding.box.xy(data.tracks)
plot(box, add = TRUE, lwd = 3)

#Retrieve bounding box points
with(box, expand.grid(xrange, yrange))



#---------------------------------------------------------------------------------
# show QQ plots for some features: lots of data points: centrale limietstelling -- gemiddelde normaal verdeeld
qqnorm(datacolumn)
qqline(datacolumn)
# homoscedasticity? (=equal variance between datasets) Use box-whisker plots
boxplot(col1, col2)

