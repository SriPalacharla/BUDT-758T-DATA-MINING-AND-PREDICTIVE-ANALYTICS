

data = read.csv('RecipeData.csv')


## 1
set.seed(1001)

num_obs = nrow(data)
sample_obs = sample(num_obs, 20)
data_sample = data[sample_obs,]

hc.sample = hclust(dist(data_sample),method='complete')
hc.sample

plot(hc.sample, main='Dendrogram', labels=data_sample[,1])
cutree(hc.sample, k=2)

## 2
set.seed(12345)
data.X = data[,-1]
km.4 = kmeans(x=data.X, centers=4, nstart=20)
km.4

table(km.4$cluster, data$cuisine)


km.3 = kmeans(x=data.X, centers=3, nstart=20)
km.3

table(km.3$cluster, data$cuisine)




