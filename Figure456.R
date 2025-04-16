
library(ggplot2)
library(grid)

A123$t=as.factor(A123$t)
A123$p=as.factor(A123$p)
levels(A123$t)=c("10","15","20","25","30")
levels(A123$p)=c("30","50","100","150","200",
                 "250","300","350","400","450","500","1000")
means$t=as.factor(means$t)
means$p=as.factor(means$p)
levels(means$t)=c("10","15","20","25","30")
levels(means$p)=c("30","50","100","150","200",
                    "250","300","350","400","450","500","1000")

#### ACC
bias.acc.grf=ggplot(A123, aes(x = p, y = bias.acc)) +
  geom_boxplot(aes(group = interaction(t, p)),color = "black", fill = "lightgray", linewidth = 0.5) +
  geom_point(size = 0.8, shape = 18, color = "black", position = position_dodge(0.5)) +
  facet_grid(rows = vars(t),scales = "free_x", space = "free_y",switch = "y") + 
  labs(x = "Sample Size", y = "Bias in Accuracy") +
  scale_y_continuous(labels = function(x) format(round(x, 1), nsmall = 1)) +
theme(plot.margin = margin(10, 10, 10, 10),
      panel.background = element_rect(fill = "grey95"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(), 
      axis.line = element_line( colour = "grey"),
      axis.text = element_text(color="black",size=10,face="italic"),
      axis.title= element_text(size=10,face="bold"),
      axis.title.x = element_text(margin = margin(t = 9)),
      axis.title.y = element_text(margin = margin(r = 9)),
      strip.text = element_text(size = 10, face = "italic"),
      plot.title = element_text(hjust = 0,size = 9, face = "bold"))+
geom_text(data = means, aes(x = p, y =-Inf, label = format(mse.acc, scientific = TRUE, digits = 4)),
            vjust = -0.5, hjust = 0.2, size = 2.3, color = "black", 
          check_overlap = TRUE, fontface = "bold")+
geom_text(data = means, aes(x = 0.6, y =-Inf, label = paste("MSE:")),
          vjust=-0.5, hjust = 0.5, size = 2, color = "black", 
          check_overlap = TRUE, fontface = "bold")+
  ggtitle("Number of Items") 

ggsave("Bias.acc.jpeg", units="in", width=8, height=6, dpi=700)

#### Sen
bias.sen.grf=ggplot(A123, aes(x = p, y = bias.sen)) +
  geom_boxplot(aes(group = interaction(t, p)),color = "black", fill = "lightgray", linewidth = 0.5) +
  geom_point(size = 0.8, shape = 18, color = "black", position = position_dodge(0.5)) +
  facet_grid(rows = vars(t), scales = "free_x", space = "free_y",switch = "y") + 
  labs(x = "Sample Size", y = "Bias in Sensitivity") +
  scale_y_continuous(labels = function(x) format(round(x, 1), nsmall = 1)) +
  theme(plot.margin = margin(10, 10, 10, 10),
        panel.background = element_rect(fill = "grey95"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(), 
        axis.line = element_line( colour = "grey"),
        axis.text = element_text(color="black",size=10,face="italic"),
        axis.title= element_text(size=10,face="bold"),
        axis.title.x = element_text(margin = margin(t = 9)),
        axis.title.y = element_text(margin = margin(r = 9)),
        strip.text = element_text(size = 10, face = "italic"),
        plot.title = element_text(hjust = 0,size = 9, face = "bold"))+
  geom_text(data = means, aes(x = p, y =-Inf, label = format(mse.sen, scientific = TRUE, digits = 4)),
            vjust = -0.5, hjust = 0.2, size = 2.3, color = "black", 
            check_overlap = TRUE, fontface = "bold")+
  geom_text(data = means, aes(x = 0.6, y =-Inf, label = paste("MSE:")),
            vjust=-0.5, hjust = 0.5, size = 2, color = "black", 
            check_overlap = TRUE, fontface = "bold")+
  ggtitle("Number of Items") 

ggsave("Bias.sen.jpeg", units="in", width=8, height=6, dpi=700)


#### Sp
bias.sp.grf=ggplot(A123, aes(x = p, y = bias.sp)) +
  geom_boxplot(aes(group = interaction(t, p)),color = "black", fill = "lightgray", linewidth = 0.5) +
  geom_point(size = 0.8, shape = 18, color = "black", position = position_dodge(0.5)) +
  facet_grid(rows = vars(t), scales = "free_x", space = "free_y",switch = "y") + 
  labs(x = "Sample Size", y = "Bias in Specificity") +
  scale_y_continuous(labels = function(x) format(round(x, 1), nsmall = 1)) +
  theme(plot.margin = margin(10, 10, 10, 10),
        panel.background = element_rect(fill = "grey95"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(), 
        axis.line = element_line( colour = "grey"),
        axis.text = element_text(color="black",size=10,face="italic"),
        axis.title= element_text(size=10,face="bold"),
        axis.title.x = element_text(margin = margin(t = 9)),
        axis.title.y = element_text(margin = margin(r = 9)),
        strip.text = element_text(size = 10, face = "italic"),
        plot.title = element_text(hjust = 0,size = 9, face = "bold"))+
  geom_text(data = means, aes(x = p, y =-Inf, label = format(mse.sp, scientific = TRUE, digits = 4)),
            vjust = -0.5, hjust = 0.2, size = 2.3, color = "black", 
            check_overlap = TRUE, fontface = "bold")+
  geom_text(data = means, aes(x = 0.6, y =-Inf, label = paste("MSE:")),
            vjust=-0.5, hjust = 0.5, size = 2, color = "black", 
            check_overlap = TRUE, fontface = "bold")+
  ggtitle("Number of Items") 


ggsave("Bias.sp.jpeg", units="in", width=8, height=6, dpi=700)





