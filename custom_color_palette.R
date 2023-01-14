library(RColorBrewer)

# Color palette 생성

# https://colorhunt.co
# popular > All time
hunt_pal_001 <- colorRampPalette(c("#222831", "#393E46", "#00ADB5", "#EEEEEE"))
hunt_pal_002 <- colorRampPalette(c("#E3FDFD", "#CBF1F5", "#A6E3E9", "#71C9CE"))
hunt_pal_003 <- colorRampPalette(c("#F9ED69", "#F08A5D", "#B83B5E", "#6A2C70"))
hunt_pal_004 <- colorRampPalette(c("#F38181", "#FCE38A", "#EAFFD0", "#95E1D3"))
hunt_pal_005 <- colorRampPalette(c("#08D9D6", "#252A34", "#FF2E63", "#EAEAEA"))
hunt_pal_006 <- colorRampPalette(c("#FFC7C7", "#FFE2E2", "#F6F6F6", "#8785A2"))
hunt_pal_007 <- colorRampPalette(c("#F9F7F7", "#DBE2EF", "#3F72AF", "#112D4E"))
hunt_pal_008 <- colorRampPalette(c("#A8D8EA", "#AA96DA", "#FCBAD3", "#FFFFD2"))
hunt_pal_009 <- colorRampPalette(c("#FFB6B9", "#FAE3D9", "#BBDED6", "#61C0BF"))
hunt_pal_010 <- colorRampPalette(c("#364F6B", "#3FC1C9", "#F5F5F5", "#FC5185"))
hunt_pal_011 <- colorRampPalette(c("#F4EEFF", "#DCD6F7", "#A6B1E1", "#424874"))
hunt_pal_012 <- colorRampPalette(c("#DEFCF9", "#CADEFC", "#C3BEF0", "#CCA8E9"))
hunt_pal_013 <- colorRampPalette(c("#1B262C", "#0F4C75", "#3282B8", "#BBE1FA"))
hunt_pal_014 <- colorRampPalette(c("#E4F9F5", "#30E3CA", "#11999E", "#40514E"))
hunt_pal_015 <- colorRampPalette(c("#F67280", "#C06C84", "#6C5B7B", "#355C7D"))
hunt_pal_016 <- colorRampPalette(c("#FCD1D1", "#ECE2E1", "#D3E0DC", "#AEE1E1"))
hunt_pal_017 <- colorRampPalette(c("#2B2E4A", "#E84545", "#903749", "#53354A"))
hunt_pal_018 <- colorRampPalette(c("#7D5A50", "#B4846C", "#E5B299", "#FCDEC0"))
hunt_pal_019 <- colorRampPalette(c("#BAD7DF", "#FFE2E2", "#F6F6F6", "#99DDCC"))
hunt_pal_020 <- colorRampPalette(c("#F8EDE3", "#BDD2B6", "#A2B29F", "#798777"))
hunt_pal_021 <- colorRampPalette(c("#212121", "#323232", "#0D7377", "#14FFEC"))
hunt_pal_022 <- colorRampPalette(c("#E23E57", "#88304E", "#522546", "#311D3F"))
hunt_pal_023 <- colorRampPalette(c("#FFCFDF", "#FEFDCA", "#E0F9B5", "#A5DEE5"))
hunt_pal_024 <- colorRampPalette(c("#FFD5CD", "#EFBBCF", "#C3AED6", "#8675A9"))
hunt_pal_025 <- colorRampPalette(c("#F7FBFC", "#D6E6F2", "#B9D7EA", "#769FCD"))
hunt_pal_026 <- colorRampPalette(c("#A8E6CF", "#DCEDC1", "#FFD3B6", "#FFAAA5"))
hunt_pal_027 <- colorRampPalette(c("#00B8A9", "#F8F3D4", "#F6416C", "#FFDE7D"))
hunt_pal_028 <- colorRampPalette(c("#2D4059", "#EA5455", "#F07B3F", "#FFD460"))
hunt_pal_029 <- colorRampPalette(c("#48466D", "#3D84A8", "#46CDCF", "#ABEDD8"))
hunt_pal_030 <- colorRampPalette(c("#FCF8E8", "#D4E2D4", "#ECB390", "#DF7861"))
hunt_pal_031 <- colorRampPalette(c("#FAF3E0", "#EABF9F", "#B68973", "#1E212D"))
hunt_pal_032 <- colorRampPalette(c("#3EC1D3", "#F6F7D7", "#FF9A00", "#FF165D"))
hunt_pal_033 <- colorRampPalette(c("#142850", "#27496D", "#0C7B93", "#00A8CC"))
hunt_pal_034 <- colorRampPalette(c("#1FAB89", "#62D2A2", "#9DF3C4", "#D7FBE8"))
hunt_pal_035 <- colorRampPalette(c("#EA907A", "#FBC687", "#F4F7C5", "#AACDBE"))
hunt_pal_036 <- colorRampPalette(c("#557571", "#D49A89", "#F7D1BA", "#F4F4F4"))


pal001 <- hunt_pal_008(10)


# Create data
data <- data.frame(
  name=c("A","B","C","D","E","AA","BB","CC","DD","EE") ,  
  value=c(9,12,5,18,45,10,17,25,35,24)
)

barplot(value ~ name, data = data, col = pal001)

