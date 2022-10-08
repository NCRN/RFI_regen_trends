library(magick)

# Fig 2 8x11
# Fig 6 8x11
# Fig 9 8x6

list.files("./results/20220325")

fig2 <- image_read("./results/20220325/results_grid_symbols_20220426_FINAL.svg")
fig2png <- image_convert(fig2, format = 'png')
image_write(fig2png, "./results/20220325/Fig2_results_grid_FINAL.png", quality = 100) # width = 1056, height = 768)


fig6 <- image_read("./results/20220325/results_grid_symbols_mg_20220426_FINAL.svg")
fig6png <- image_convert(fig6, format = 'png')
image_write(fig6png, "./results/20220325/Fig6_results_grid_mg_FINAL.png", quality = 100)


fig9 <- image_read("./results/20220325/results_grid_symbols_GETT_FINAL.svg")
fig9png <- image_convert(fig9, format = 'png')
image_write(fig9png, "./results/20220325/Fig9_results_grid_GETT_FINAL.png", quality = 100)


