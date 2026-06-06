#Xử lý dữ liệu 
## 1. Cài đặt và load các thư viện cần thiết
install.packages("readxl")      #Đọc file Excel (.xls, .xlsx).
install.packages("dplyr")       #Xử lý, lọc, sắp xếp và biến đổi dữ liệu dạng bảng.
install.packages("cluster")     #Các thuật toán phân cụm
install.packages("factoextra")  #Trực quan hóa kết quả phân tích đa biến 
install.packages("ggplot2")     #Vẽ đồ thị 
install.packages("tidyr")       #Chuyển đổi và sắp xếp dữ liệu
install.packages("forcats")     #Xử lý biến phân loại (factor)
install.packages("tibble")      #Dạng bảng dữ liệu hiện đại
install.packages("fastDummies") #Tạo biến giả (dummy variables) từ biến phân loại.
install.packages("FactoMineR")  #Phân tích đa biến (PCA, MCA, CA,...).
install.packages("clustMixType")# Phân cụm dữ liệu hỗn hợp (liên tục + phân loại).
install.packages("reshape2")    #Chuyển đổi định dạng dữ liệu
install.packages("tidyverse")   #Bộ công cụ xử lý dữ liệu
install.packages("ggfortify")   #Trực quan hóa các mô hình thống kê với ggplot2
install.packages("knitr")       #Tạo báo cáo động từ R (với Markdown, LaTeX,...).
install.packages("rmarkdown")   #Kết hợp văn bản và mã R để tạo báo cáo HTML/PDF/Word.

library(data.table)
library(readxl)
library(dplyr)
library(cluster)
library(factoextra)
library(ggplot2)
library(tidyr)
library(forcats)
library(tibble)
library(fastDummies)
library(FactoMineR)
library(clustMixType)
library(reshape2)
library(tidyverse)
library(ggfortify)
library(knitr)
library(rmarkdown)

#2. Đọc dữ liệu
df <- read_excel("D:/DataLapDesk.xlsx")
head(df) # Xuất 6 dòng dữ liệu đầu tiên

#3. Làm sạch dữ liệu (nếu cần)
#3.1 Xóa khoảng trắng thừa và đưa các cột cần thiết về dạng số (numeric )
df <- df %>%
mutate(across(c(Price, Quantity, Total), as.numeric))

#3.2 Tách các cột numeric ra riêng (vì chúng ta vừa xử lý dữ liệu số vừa xử lý dữ liệu chuỗi)
df_num <- df %>% select(where(is.numeric))

#3.3 Tách cột định danh / chuỗi để giữ lại
#Các cột phân loại (ví dụ: giới tính, nhóm ngành,…) sẽ được chuyển thành các cột dummy (0 hoặc 1).Dùng biến giả đối với dữ liệu không phải số.
df_full <- dummy_cols(df, remove_first_dummy = TRUE, remove_selected_columns = TRUE)

#3.4 Kiểm tra lại cấu trúc dữ liệu và NA
#Cấu trúc dữ liệu 
str(df)    
#Tóm tắt thống kê của dữ liệu
summary(df)

#4. Tổng hợp theo Branch_Name
branch_sales_summary <- df %>%
  group_by(Branch_Name) %>% #Nhóm theo Branch_Name 
  summarise(Total_Revenue = sum(Total, na.rm=TRUE), #Tính tổng doanh thu theo Branch_Name 
            Avg_Quantity = mean(Quantity, na.rm=TRUE), #Tính trung bình số lượng theo Branch_Name 
            Total_Sales = n(), #Tính tổng giao dịch bán ra theo Branch_Name 
            Unique_Products = n_distinct(Product_Name)) %>% # Đếm số lượng sản phẩm khác nhau đã bán theo Branch_Name 
  
  ## Đưa cột Branch_Name thành index (tên hàng) của data frame kết quả
  column_to_rownames(var = "Branch_Name")
## Hiển thị kết quả 
head(branch_sales_summary)

# Phân cụm theo thương hiệu (Brand_Name)
library(cluster) ## 1. Tính khoảng cách Gower (dữ liệu nhiều loại biến khác nhau ) - Thông tin: Khoảng cách Gower là một phép đo khoảng cách dùng để tính độ tương đồng giữa các quan sát có dữ liệu hỗn hợp(số, phân loại,nhị phân)
gower_dist <- daisy(branch_sales_summary, metric = "gower") ## 2. Phân cụm phân cấp(Hierarchical Clustering) - Phương pháp ward.D2 để gộp cụm có độ đồng nhất cao.
cluster_result <- hclust(gower_dist, method = "ward.D2") ## 3. Vẽ cây phân cấp(thể hiện cách các nhóm được gộp dần theo khoảng cách)
plot(cluster_result, labels = rownames(branch_sales_summary), sub = "", xlab = "")
rect.hclust(cluster_result, k = 4, border = "red") #vẽ khung bao quanh 4 cụm 
cluster_labels <- cutree(cluster_result, k = 4) ## 5. Gắn nhãn cụm vào dữ liệu gốc ### 5.1 Tạo bảng mới cluster_df gồm 2 cột (Branch_Name và Cluster)
cluster_df <- data.frame( Branch_Name = rownames(branch_sales_summary), Cluster = cluster_labels ) ## 5.2 Nối vào df qua cột Branch_Name - Hàm left_join(…, by = “…”) để ghép hai bảng dựa trên cột chung là Branch_Name.
df_clustered_branchname <- df %>% left_join(cluster_df, by = "Branch_Name")
# Xuất 6 dòng đầu để kiểm tra 
head(df_clustered_branchname)

#Phân cụm theo bản ghi gốc
library(clustMixType)
#1. Chọn các cột phù hợp (gồm cả char và num)
#Tự động chuyển tất cả các cột kiểu ký tự (character) thành factor vì dữ liệu cần phân cụm gồm cả dữ liệu số và dữ liệu phân loại .
df_clustering <- df %>%
  select(Month,Saler_Name,Branch_Name,Product_Name,Price,Product_Brand, Product_Type,Quantity,Total) %>%
  mutate(across(where(is.character), as.factor)) 

#2. Chạy phân cụm K-Prototypes với k cụm (ví dụ 4)
set.seed(123) 
kproto_result <- kproto(df_clustering, k = 4)

#3. Gắn nhãn cụm vào dữ liệu gốc
df$Cluster <- kproto_result$cluster

#4. Kiểm tra
summary(kproto_result)

#5. Tóm tắt đặc trưng mỗi cụm bằng cách tính trung bình của tất cả các cột số trong từng cụm.
df %>%group_by(Cluster) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))
#6. Trực quan hóa 
# 6.1 Vẽ để phân tích Total, Price, Quantity từng cụm

library(ggplot2)
library(scales)

#Tính tổng Total theo từng cụm
# Tính tổng theo cụm
df_total <- df %>%
  group_by(Cluster) %>%
  summarise(total_total = sum(Total, na.rm = TRUE))

# Vẽ biểu đồ cột thể hiện tổng Total theo cụm
plot_total<- ggplot(df_total, aes(x = factor(Cluster), y = total_total, fill = factor(Cluster))) +
  geom_col(width = 0.6) +
  geom_text(aes(label = comma(total_total)),
            vjust = -0.5, color = "black", fontface = "bold", size = 3.5) +
  scale_y_continuous(labels = comma) +
  labs(title = "Tổng Total theo từng cụm",
       x = "Cụm", y = "Tổng Total") +
  theme_minimal()
plot_total


#Biểu đồ hộp cho Price
plot_price <- ggplot(df, aes(x = factor(Cluster), y = Price, fill = factor(Cluster))) +
  geom_boxplot() +
  stat_summary(fun = median, geom = "text",
               aes(label = comma(..y..)),
               vjust = 0.9, color = "black", fontface = "bold",size=2.5) +
  # Để hiển thị giá trị y trực quan hơn
  scale_y_continuous(labels = comma) + 
  labs(title = "Phân bố Price theo cụm",
       x = "Cụm", y = "Price") +
  theme_minimal()
plot_price

#Biểu đồ hộp cho Quantity
# Tính tổng số lượng, Max,Min của mỗi đơn trong mỗi cụm. 
df_summary <- df %>%
  group_by(Cluster) %>%
  summarise(
    total_quantity = sum(Quantity, na.rm = TRUE),
    min_quantity = min(Quantity, na.rm = TRUE),
    median_quantity = median(Quantity, na.rm = TRUE),
    max_quantity = max(Quantity, na.rm = TRUE)
  )
# Vẽ đồ thị thể hiện 
plot_quantity <- ggplot(df, aes(x = factor(Cluster), y = Quantity, fill = factor(Cluster))) +
  geom_boxplot(outlier.shape = NA) +  # bỏ chấm outlier nếu rối
  geom_text(data = df_summary,
            aes(x = factor(Cluster), y = median_quantity,
                label = paste0("Median: ", comma(median_quantity))),
            color = "black", fontface = "bold", size = 3,
            inherit.aes = FALSE, vjust = 1.22) +
  # Thêm nhãn min count mỗi cụm
  geom_text(data = df_summary,
            aes(x = factor(Cluster), y = min_quantity,
                label = paste0("Min: ", comma(min_quantity))),
            color = "blue", fontface = "italic", size = 2.8,
            inherit.aes = FALSE, vjust = 1.2) +
  # Thêm nhãn max count mỗi cụm
  geom_text(data = df_summary,
            aes(x = factor(Cluster), y = max_quantity,
                label = paste0("Max: ", comma(max_quantity))),
            color = "red", fontface = "italic", size = 2.8,
            inherit.aes = FALSE, vjust = -0.8) +
  # Thêm nhãn tổng mỗi cụm
  geom_text(data = df_summary,
            aes(x = factor(Cluster), y = max_quantity * 1.05,
                label = paste0("Tổng: ", comma(total_quantity))),
            color = "purple", fontface = "bold", size = 3,
            inherit.aes = FALSE) +
  scale_y_continuous(labels = comma) +
  labs(title = "Phân bố Quantity theo cụm",
       x = "Cụm", y = "Quantity") +
  theme_minimal()
plot_quantity

# Biểu đồ hộp cho Product_Name
library(dplyr)
library(ggplot2)
library(scales)

# Đếm số lượng từng Product_Name theo Cluster
df_counts <- df %>%
  group_by(Cluster, Product_Name) %>%
  summarise(count = n(), .groups = 'drop')

# Lấy max count mỗi cụm
df_max_count <- df_counts %>%
  group_by(Cluster) %>%
  filter(count == max(count)) %>%
  ungroup()
# Lấy min count mỗi cụm
df_min_count <- df_counts %>%
  group_by(Cluster) %>%
  slice_min(order_by = count, n = 1, with_ties = FALSE)  # chọn Product_Name có count nhỏ nhất trong mỗi cụm

# Vẽ boxplot số lượng sản phẩm theo Cluster
plot_pro_name <-ggplot(df_counts, aes(x = factor(Cluster), y = count, fill = factor(Cluster))) +
  geom_boxplot() +
  stat_summary(fun = median, geom = "text",
               aes(label = comma(..y..)),
               vjust = -0.5, color = "black", fontface = "bold", size=3) +
# Thêm nhãn max count mỗi cụm
  geom_text(data = df_max_count,
            aes(x = factor(Cluster), y = count, label = paste0(Product_Name, ": ", count)),
            vjust = -1.2, color = "red", fontface = "bold", size=3,
            position = position_dodge(width = 0.75)) +
# Thêm nhãn min count mỗi cụm
  geom_text(data = df_min_count,
            aes(x = factor(Cluster), y = count, label = paste0(Product_Name, ": ", count)),
            vjust = 1.5, color ="black", fontface = "bold", size=3,
            position = position_dodge(width = 0.75)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Phân bố số lượng Product_Name theo cụm",
       x = "Cụm", y = "Số lượng (count)") +
  theme_minimal()
plot_pro_name


#Biểu đồ hộp cho Month
# Đếm số đơn theo cụm và tháng
df_counts <- df %>%
  group_by(Cluster, Month) %>%
  summarise(count = n(), .groups = 'drop')

# Lấy tháng có nhiều đơn nhất mỗi cụm (có thể nhiều tháng trùng count → giữ hết)
df_max_count <- df_counts %>%
  group_by(Cluster) %>%
  filter(count == max(count)) %>%
  ungroup()

# Lấy tháng có ít đơn nhất mỗi cụm
df_min_count <- df_counts %>%
  group_by(Cluster) %>%
  filter(count == min(count)) %>%
  ungroup()

# Vẽ boxplot
plot_month <- ggplot(df, aes(x = factor(Cluster), y = Month, fill = factor(Cluster))) +
  geom_boxplot() +
  stat_summary(fun = median, geom = "text",
               aes(label = comma(..y..)),
               vjust = 1.5, color = "black", fontface = "bold", size = 2.5) +
# Thêm nhãn max count mỗi cụm
  geom_text(data = df_max_count,
            aes(x = factor(Cluster), y = Month,
                label = paste0("Max: ", Month, " (", count, ")")),
            vjust = -0.8, color = "black", fontface = "bold", size = 3,
            position = position_nudge(x = 0.2)) +
# Thêm nhãn min count mỗi cụm
  geom_text(data = df_min_count,
            aes(x = factor(Cluster), y = Month,
                label = paste0("Min: ", Month, " (", count, ")")),
            vjust = 1.2, color = "blue", fontface = "bold", size = 3,
            position = position_nudge(x = -0.2)) +
  
  scale_y_continuous(breaks = 1:12) +
  labs(title = "Phân bố Month theo cụm",
       x = "Cụm", y = "Month") +
  theme_minimal()

plot_month

#Nhân viên

# Đếm số lượng từng Nhân viên theo Cluster
df_counts <- df %>%
  group_by(Cluster, Saler_Name) %>%
  summarise(count = n(), .groups = 'drop')

# Lấy max count mỗi cụm
df_max_count <- df_counts %>%
  group_by(Cluster) %>%
  filter(count == max(count)) %>%
  ungroup()

# Lấy min count mỗi cụm
df_min_count <- df_counts %>%
  group_by(Cluster) %>%
  slice_min(order_by = count, n = 1, with_ties = FALSE)

# Vẽ boxplot số lượng sản phẩm theo Cluster
plot_saler_name <-ggplot(df_counts, aes(x = factor(Cluster), y = count, fill = factor(Cluster))) +
  geom_boxplot() +
  stat_summary(fun = median, geom = "text",
               aes(label = comma(..y..)),
               vjust = -0.5, color = "black", fontface = "bold", size=3) +
# Thêm nhãn max count mỗi cụm
  geom_text(data = df_max_count,
            aes(x = factor(Cluster), y = count, label = paste0(Saler_Name, ": ", count)),
            vjust = -1.2, color = "red", fontface = "bold", size=3,
            position = position_dodge(width = 0.75)) +
# Thêm nhãn min count mỗi cụm
  geom_text(data = df_min_count,
            aes(x = factor(Cluster), y = count, label = paste0(Saler_Name, ": ", count)),
            vjust = 1.5, color ="black", fontface = "bold", size=3,
            position = position_dodge(width = 0.75)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Phân bố nhân viên bán hàng theo cụm",
       x = "Cụm", y = "Số lượng (count)") +
  theme_minimal()
plot_saler_name

# Phân tích theo cụm

#1. Tháng được mua nhiều nhất mỗi cụm
plot_month


#2. Sản phẩm bán chạy nhất mỗi cụm
plot_pro_name

# 3. Nhân viên bán chạy nhất mỗi cụm
plot_saler_name

# 4. Doanh thu theo cụm
plot_total

# 5. Tổng đơn hàng theo từng cụm, max/ min của 1 đơntheo từng cụm.
plot_quantity


#6. Sản phẩm ít được mua nhất
library(dplyr)
df_min_products <- df %>%
  group_by(Branch_Name, Product_Name) %>%
  summarise(total_quantity = sum(Quantity, na.rm = TRUE), .groups = "drop") %>%
  group_by(Branch_Name) %>%
  slice_min(order_by = total_quantity, n = 1, with_ties = TRUE)  # Lấy sản phẩm ít nhất mỗi chi nhánh
df_min_products

#7. Sản phẩm được mua nhiều nhất từng chi nhánh
df_max_products <- df %>%
  group_by(Branch_Name, Product_Name) %>%
  summarise(total_quantity = sum(Quantity, na.rm = TRUE), .groups = "drop") %>%
  group_by(Branch_Name) %>%
  slice_max(order_by = total_quantity, n = 1, with_ties = TRUE)  # Lấy sản phẩm nhiều nhất mỗi chi nhánh
df_max_products

#8. Nhân viên bán chạy trong năm
#Ý tưởng: sắp xếp theo doanh thu theo thứ tự giảm dần và lấy 5 bản ghi đầu tiên
top5_salers <- df %>%
  group_by(Saler_Name) %>%
  summarise(total_quantity = sum(Quantity, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_quantity)) %>%
  slice_head(n = 5)
top5_salers

#9.Nhân viên bán kém nhất trong năm
#Ý tưởng: sắp xếp theo doanh thu theo thứ tự mặc định là tăng dần và lấy 3 bản ghi đầu tiên.
top3_salers <- df %>%
  group_by(Saler_Name) %>%
  summarise(total_quantity = sum(Quantity, na.rm = TRUE), .groups = "drop") %>%
  arrange(total_quantity) %>%
  slice_head(n = 3)
top3_salers

#10 . Nhân viên bán thấp hơn trung bình
#Ý tưởng: Tính số lượng sản phẩm bán được trên tổng số người bán để lấy 1 mức trung bình, những nhân viên có tổng lượng hàng bán dưới trung bình cần được thúc đẩy nâng cao năng suất, hiệu quả.
saler_quantity <- df %>%
  group_by(Saler_Name) %>%
  summarise(total_quantity = sum(Quantity, na.rm = TRUE), .groups = "drop")
mean_quantity <- mean(saler_quantity$total_quantity)
below_average_salers <- saler_quantity %>%
  filter(total_quantity < mean_quantity)
below_average_salers

#11. Tổng số lượng bán ra của từng hãng và tháng bán chạy nhất
#Tổng số lượng của từng hãng 
brand_quantity <- df %>%
  group_by(Product_Brand) %>%
  summarise(total_quantity = sum(Quantity, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_quantity))
#In ra kết quả 
brand_quantity

#Tháng bán chạy nhất 
monthly_brand_quantity <- df %>%
  group_by(Product_Brand, Month) %>% # Nhóm theo hãng và tháng
  summarise(monthly_quantity = sum(Quantity, na.rm = TRUE), .groups = "drop_last") %>%
  slice_max(order_by = monthly_quantity, n = 1) %>% # Chọn tháng có số lượng bán cao nhất cho mỗi hãng
  ungroup()

print(monthly_brand_quantity)

#12. Doanh số theo tháng
#Thống kê doanh thu theo tháng
month_revenue <- df %>%
  group_by(Month) %>%
  summarise(Tổng_doanh_thu  = sum(Total, na.rm = TRUE), .groups = "drop")
month_revenue

#Vẽ đồ thị 
ggplot(month_revenue, aes(x = factor(Month), y = Tổng_doanh_thu)) +
  geom_col(fill = "orange") +
  geom_text(aes(label = scales::comma(Tổng_doanh_thu, big.mark = ".", decimal.mark = ",")), # Hiển thị số có dấu chấm phân cách hàng nghìn
            vjust = -0.5, # Điều chỉnh vị trí của nhãn so với cột
            size = 3) + # Kích thước của chữ số
  labs(title = "Doanh thu theo tháng",
       x = "Tháng",
       y = "Tổng doanh thu") +
  scale_y_continuous(labels = scales::comma) + # Định dạng lại trục Y để không có "e"
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))

# 13. Best Seller mỗi tháng
#  Tính sản phẩm bán chạy nhất mỗi tháng (dựa trên Tổng doanh thu)
best_seller_monthly <- df %>%
  group_by(Month, Product_Name) %>% # Nhóm theo tháng và sản phẩm
  summarise(Doanh_thu_san_pham = sum(Total, na.rm = TRUE), .groups = "drop_last") %>% # Tính tổng doanh thu cho từng sản phẩm trong tháng
  slice_max(order_by = Doanh_thu_san_pham, n = 1) %>% # Chọn sản phẩm có doanh thu cao nhất cho mỗi tháng
  ungroup() # Bỏ nhóm để có một dataframe phẳng

# Hiển thị kết quả sản phẩm bán chạy nhất mỗi tháng
print(best_seller_monthly,12)

#14. Đánh giá doanh thu theo tháng, quý, xem xét mức độ tăng trưởng
library(dplyr)
library(lubridate) # Để xử lý ngày tháng
library(tidyr)

df_14 <- df %>%
  mutate(
    Year = year(Date),
    Month = month(Date),
    Quarter = quarter(Date)
  )
# 1. Tính toán doanh thu theo tháng
month_revenue_full_date <- df_14 %>%
  group_by(Year, Month) %>%
  summarise(Tong_doanh_thu = sum(Total, na.rm = TRUE), .groups = "drop") %>%
  arrange(Year, Month)

# 2. Tính toán tăng trưởng doanh thu so với tháng trước
month_revenue_growth <- month_revenue_full_date %>%
  mutate(
    Doanh_thu_thang_truoc = lag(Tong_doanh_thu, n = 1),
    Tang_truong_thang = (Tong_doanh_thu - Doanh_thu_thang_truoc) / Doanh_thu_thang_truoc * 100
  )

print(month_revenue_growth)
# Biểu đồ đường
library(ggplot2)
ggplot(month_revenue_growth, aes(x = Month, y = Tang_truong_thang)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(color = "black", size = 2) +
  labs(
    title = "Tăng trưởng doanh thu theo tháng (%)",
    x = "Thời gian",
    y = "Tăng trưởng (%)"
  ) +
  scale_x_continuous(breaks = 1:12, labels = paste0("Tháng ", 1:12)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Tính toán doanh thu theo quý và tăng trưởng so với quý trước
quarter_revenue_full_date <- df_14 %>%
  group_by(Year, Quarter) %>%
  summarise(Tong_doanh_thu_quy = sum(Total, na.rm = TRUE), .groups = "drop") %>%
  arrange(Year, Quarter)

quarter_revenue_growth <- quarter_revenue_full_date %>%
  mutate(
    Doanh_thu_quy_truoc = lag(Tong_doanh_thu_quy, n = 1),
    Tang_truong_quy = (Tong_doanh_thu_quy - Doanh_thu_quy_truoc) / Doanh_thu_quy_truoc * 100
  )

print(quarter_revenue_growth)

## Vẽ biểu đồ
ggplot(quarter_revenue_growth, aes(x = Quarter, y = Tang_truong_quy, group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "black", size = 3) +
  geom_text(aes(label = round(Tang_truong_quy, 2)), vjust = -1, size = 4, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = 1:4, labels = paste0("Quý ", 1:4)) +
  scale_y_continuous(limits = c(-5, 6), breaks = seq(-5, 6, by = 1)) +
  labs(
    title = "Tăng trưởng doanh thu theo quý (%)",
    x = "Quý",
    y = "Tăng trưởng (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )

#Dự báo
#1. Dự báo doanh thu năm tới
library(dplyr)
library(lubridate)
library(forecast)
library(ggplot2)

# 1. Tổng hợp doanh thu theo tháng
df_with_year_col <- df %>%
  mutate(Year = year(Date)) 

monthly_sales <- df %>%
  mutate(Date = floor_date(as.Date(Date), "month")) %>% # Đảm bảo cột Date ở định dạng ngày tháng và làm tròn về đầu tháng
  group_by(Date) %>%
  summarise(Total_Revenue = sum(Total, na.rm = TRUE), .groups = "drop") %>%
  arrange(Date)

# Chuyển đổi thành đối tượng chuỗi thời gian (ts object)
start_year <- year(min(monthly_sales$Date))
start_month <- month(min(monthly_sales$Date))

sales_ts <- ts(monthly_sales$Total_Revenue,
               start = c(start_year, start_month),
               frequency = 12) # Tần suất 12 cho dữ liệu hàng tháng

# 2. Phân tích chuỗi thời gian
# Trực quan doanh thu theo tháng với nhãn thời gian chi tiết
ggplot(monthly_sales, aes(x = Date, y = Total_Revenue)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Doanh thu hàng tháng theo thời gian",
       x = "Tháng",
       y = "Doanh thu") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Lựa chọn và huấn luyện mô hình (ví dụ: auto.arima)
# Huấn luyện mô hình ARIMA
fit_arima <- auto.arima(sales_ts)
summary(fit_arima)

# Tạo dự báo
forecast_arima <- forecast(fit_arima, h = 12)

# Vẽ biểu đồ dự báo ARIMA
autoplot(forecast_arima) +
  labs(title = "Dự báo doanh thu hàng tháng (ARIMA)",
       y = "Doanh thu",
       x = "Thời gian") +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal()

# Tương tự với ETS
fit_ets <- ets(sales_ts)
forecast_ets <- forecast(fit_ets, h = 12)

# Biểu đồ ETS
autoplot(forecast_ets) +
  labs(title = "Dự báo doanh thu hàng tháng (ETS)",
       y = "Doanh thu",
       x = "Thời gian") +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Tạo bảng so sánh giữa ARIMA và ETS
library(lubridate)
library(scales)

future_months <- seq(from = as.Date(max(monthly_sales$Date)) %m+% months(1), 
                     by = "month", 
                     length.out = 12)

forecast_compare <- data.frame(
  Tháng = format(future_months, "%Y-%m"),
  Du_bao_ARIMA = formatC(as.numeric(forecast_arima$mean), format = "f", big.mark = ".",decimal.mark = ",", digits = 0),
  Du_bao_ETS = formatC(as.numeric(forecast_ets$mean), format = "f", big.mark = ".",decimal.mark = ",", digits = 0)
)

# Hiển thị bảng
library(knitr)
kable(forecast_compare, caption = "So sánh dự báo doanh thu giữa mô hình ARIMA và ETS")

#2.Dự báo nhu cầu hàng hóa
#1. Tiền xử lí dữ liệu
library(dplyr)

#Tổng hợp doanh số theo chi nhánh và sản phẩm
sales_by_branch <- df %>%
  group_by(Branch_Name,Product_Brand) %>%
  summarise(Total_Sales = sum(Total),Avg_Quantity = mean(Quantity))

#Kiểm tra dữ liệu
head(sales_by_branch)

#2. Dự báo nhu cầu khách hàng theo chi nhánh
library(forecast)
#Chuyển đổi dữ liệu thành chuỗi thời gian 
start_year <- year(min(monthly_sales$Date))
start_month <- month(min(monthly_sales$Date))

df_ts <- ts(sales_by_branch$Avg_Quantity,
            start = c(start_year, start_month), 
            frequency = 12)

#Xây dựng mô hình ARIMA
model_arima <- auto.arima(df_ts)

#Dự báo nhu cầu hàng hóa trong 6 tháng tới
forecast_values <- forecast(model_arima, h=6)

#Hiển thị kết quả
autoplot(forecast_values)

#3.Tối ưu mức tồn kho bằng machine learning Random Forest
library(randomForest)
#Chuẩn bị dữ liệu cho mô hình 
inventory_data <- df %>%
  select(Branch_Name,Product_Brand, Quantity, Total)

#Chia tập dữ liệu thành tập huấn luyện và kiểm tra
set.seed(123)
train_index <- sample(1:nrow(inventory_data),0.7*nrow(inventory_data))
train_data <- inventory_data[train_index,]
test_data <- inventory_data[-train_index,]

#Xây dựng mô hình Random Forest 
model_rf <- randomForest(Quantity ~ Branch_Name+Product_Brand+Total, data=train_data,ntree=50)
#Dự đoán mức tồn kho tối ưu
predictons <- predict(model_rf,test_data)

#Kiểm tra kết quả
plot(predictons, test_data$Quantity)
cor(predictons, test_data$Quantity)
library(dplyr)

# Tạo danh sách lưu dự báo theo từng chi nhánh
forecast_list <- lapply(unique(sales_by_branch$Branch_Name), function(branch) {
  # Lọc dữ liệu theo chi nhánh
  branch_data <- sales_by_branch %>% filter(Branch_Name == branch)
  
  # Chuyển đổi dữ liệu thành chuỗi thời gian
  df_ts <- ts(branch_data$Avg_Quantity, frequency = 12)
  
  # Xây dựng mô hình ARIMA
  model_arima <- auto.arima(df_ts)
  
  # Dự báo nhu cầu trong 6 tháng tới
  forecast_values <- forecast(model_arima, h=6)
  
  # Lưu kết quả dự báo
  data.frame(Branch_Name = branch, Forecasted_Quantity = forecast_values$mean)
})

# Gộp danh sách lại thành bảng
forecast_table <- bind_rows(forecast_list)

# Hiển thị kết quả
print(forecast_table)

#4.Kiểm tra hiệu suất dự báo
library(ggplot2)

#So sánh dự báo với dữ liệu thực tế
df$Predicted_Inventory <- predict(model_rf, df)

ggplot(df, aes(x = Quantity, y = Predicted_Inventory)) + 
  geom_point(color = "blue")+
  geom_abline(intercept = 0, slope = 1, color = "red")+
  ggtitle("So sánh mức tồn kho dự báo và thực tế")

