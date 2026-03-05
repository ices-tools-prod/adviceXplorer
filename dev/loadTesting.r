# library(loadtest)
# results <- loadtest(url = "https://sag.ices.dk/SAG_API/api/StockList?year=2023",
#                     method = "GET",
#                     # headers = c("version"="v1.0"),
#                     # body = list(sentences = list("I love this band")),
#                     # encode="json",
#                     threads = 8,
#                     loops = 32,
#                     delay_per_request=100)
# loadtest_report(results,"D:/report/report_stocklist.html")
# plot_elapsed_times(results)
# plot_elapsed_times_histogram(results)
# plot_requests_by_thread(results)
# plot_requests_per_second(results)