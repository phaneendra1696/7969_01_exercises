# import pandas, numpy
# Create the required data frames by reading in the files

# Q1 Find least sales amount for each item
# has been solved as an example
def least_sales(df):
    # write code to return pandas dataframe
	ls = df.groupby(["Item"])["Sale_amt"].min().reset_index()
    return ls

# Q2 compute total sales at each year X region
def sales_year_region(df):
    # write code to return pandas dataframe
	df['OrderDate']=pd.to_datetime(df['OrderDate'])
	df['year']=df['OrderDate'].dt.year
	ls=df.groupby(['year','Region']).sum()
	return ls
# Q3 append column with no of days difference from present date to each order date
def days_diff(df):
    # write code to return pandas dataframe
	reference_date=datetime.datetime(2020,1,1)
	df['days_diff']=reference_date-df['OrderDate']
	return df
# Q4 get dataframe with manager as first column and  salesman under them as lists in rows in second column.
def mgr_slsmn(df):
    # write code to return pandas dataframe
	manager=df['Manager'].unique()
	salesmen=[]
	for i in range(0,df['Manager'].nunique()):
		salesmen.append(df[df['Manager']==manager[i]]['SalesMan'].unique().tolist())
	df1=pd.DataFrame(zip(manager,salesmen),columns=['Manager','salesman'])
	return df1

# Q5 For all regions find number of salesman and number of units
def slsmn_units(df):
    # write code to return pandas dataframe
	ls=df.groupby('Region').agg({'Sale_amt':'sum','SalesMan':'count'})
	return ls
# Q6 Find total sales as percentage for each manager
def sales_pct(df):
    # write code to return pandas dataframe
	q10=df.groupby('Manager').sum()['Sale_amt'].to_frame()
	total=sum(q10['Sale_amt'])
	q10['Sale_amt']=df2['Sale_amt']/total
	q10.columns=['percentage_sales']
	return q10

# Q7 get imdb rating for fifth movie of dataframe
def fifth_movie(df):
	# write code here
	return df.iloc[4]['imdbRating']

# Q8 return titles of movies with shortest and longest run time
def movies(df):
	# write code here
	ls=df[(df['duration']==min(df['duration'])) | (df['duration']==max(df['duration']))]
	return ls
# Q9 sort by two columns - release_date (earliest) and Imdb rating(highest to lowest)
def sort_df(df):
	# write code here
	ls=df.sort_values(by=['year','imdbRating'],ascending=[True,False])
	return ls
# Q10 subset revenue more than 2 million and spent less than 1 million & duration between 30 mintues to 180 minutes
def subset_df(df):
	# write code here
	ls=df[(df['duration']>30) & (df['duration']<180)]
	return ls
# Q11 count the duplicate rows of diamonds DataFrame.
def dupl_rows(df):
	# write code here
	ls=df.duplicated().value_counts()
	return ls
# Q12 droping those rows where any value in a row is missing in carat and cut columns
def drop_row(df):
	# write code here
	df=df.dropna(subset=['carat','cut'])
	return df
# Q13 subset only numeric columns
def sub_numeric(df):
	# write code here
	ls=df.select_dtypes(include='number')
	return ls
# Q14 compute volume as (x*y*z) when depth > 60 else 8
def volume(df):
	# write code here
	df['z']=df['z'].apply(pd.to_numeric,errors='coerce')
	df['volume']=df.apply(lambda df: df['x']*df['y']*df['z'] if df['depth']>60 else 8,axis=1)
	return df
# Q15 impute missing price values with mean
def impute(df):
	# write code here
	df=df.fillna(df.mean())
	return df

#Bonus1
def bonus1(df):
	genre_combo=[]
	for i in df.iterrows():
    temp=""
    for j in range(16,44):
        if i[1][j]==1:
            temp+=str(columns[j-16])+" "
    	genre_combo.append(temp)
	df['Genre_combo']=genre_combo
	ls=df.groupby(['year','type','Genre_combo']).agg({'imdbRating':['min','max','mean'],'duration':'sum'})
	return ls
#Bonus2
def bonus2(df):
	df['length']=df['wordsInTitle'].apply(lambda x:len(x))
	df2=df.groupby('year').agg({'length':'mean'}).reset_index()
	quant=df2['length'].quantile([0.25,0.5,0.75])
	df2['percentile']=df2['length'].apply(lambda x:1 if x<quant[0.25] else(2 if x<quant[0.5] and x>quant[0.25] else(3 if x>quant[0.5] and x<quant[0.75] else 4)))
	df1=pd.pivot_table(df2,values=['length'],index='year',columns=['percentile'],aggfunc={'length':'count'},fill_value=0)
	df1[['max','min']]=df2.groupby('year').agg({'length':[min,max]})
	return df1
	
#Bonus 3
def bonus2(d):
	d['z']=d['z'].apply(pd.to_numeric,errors='coerce')
	d['volume']=d.apply(lambda row: row['x']*row['y']*row['z'] if row['depth']>60 else 8,axis=1)
	qc=pd.qcut(list(d['volume']),q=5,precision=1)
	bins=qc.codes
	dtype = pd.CategoricalDtype(['1','2','3','4','5'], ordered=True)
	codes = pd.Categorical.from_codes(bins, dtype=dtype)
	df1=pd.crosstab(d['cut'],codes,normalize=True)
	return df1
	
#Bonus4
def bonus4(df):
	grouped=df.groupby(['title_year'])
    	top10_gross=grouped.apply(lambda x: x.sort_values(by='gross',ascending=False).head(1 if int(0.1*len(x))==0 else int(0.1*len(x)) )).reset_index(drop=True)
    	top10_grouped=top10_gross.groupby(['title_year','genres'])
    	top_movies=top10_grouped.agg({'imdb_score':'mean'})
    	top_movies['no.of top movies']=top10_grouped.apply(lambda x:len(x))
    	return top_movies

#Bonus5
def bonus5(df):
	df['decile']=pd.qcut(imdb['duration'],10,labels=np.arange(1,11,1))
   	 grpby_obj=df.groupby('decile')
   	 imdb1=grpby_obj.agg({'nrOfNominations':sum,'nrOfWins':sum,'fn':'count'})
    	genrestab=df.iloc[:,np.r_[8,17:45]]
    	z=genrestab.groupby('decile')[genrestab.columns.tolist()[1:28]].sum().transpose()
    	e=pd.DataFrame(z.apply(lambda x: x.nlargest(3).index,axis=0).transpose(),)
    	e.columns=["1","2","3"]
    	imdb1['top3_genre']=e["1"]+";"+e["2"]+";"+e["3"]
    	return imdb1
