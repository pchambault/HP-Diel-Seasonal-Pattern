#######################################################################
###########         functions to generate tables with   ###############
############             embbeded figures with gt       ###############
#######################################################################

#---------------------------------------------------------------------------------
# https://themockup.blog/posts/2020-10-31-embedding-custom-features-in-gt-tables/
gt_plot <- function(table_data, plot_col, data_col, plot_fun, ...) {
  # save the data extract ahead of time
  # to be used in our anonymous function below
  data_in <- purrr::pluck(table_data, "_data", data_col)
  
  text_transform(
    table_data,
    # note the use of {{}} here - this is tidy eval
    # that allows you to indicate specific columns
    locations = cells_body(columns = c({{ plot_col }})),
    fn = function(x) {
      plot <- purrr::map(data_in, 
                         plot_fun, 
                         width = 300, 
                         height = 70, 
                         same_lim = TRUE, 
                         col = "darkgray", ...)
      plot_svg <- purrr::map(plot, "svg_text")
      purrr::map(plot_svg, gt::html)
    }
  )
}




#--------------------------------------------
# gt_plt_bar_stack_extra: generate bar_plot
#--------------------------------------------
gt_plt_bar_stack_extra <- function (gt_object,
                                    column = NULL,
                                    palette = c("#ff4343", "#bfbfbf",
                                                "#0a1c2b", "#e1be6a"),
                                    labels = c("Group 1", "Group 2", "Group 3", "Group 4"),
                                    position = "fill",
                                    width = 70,
                                    fmt_fn = scales::label_number(scale_cut = cut_short_scale(),
                                                                  trim = TRUE))
{
  stopifnot(`Table must be of class 'gt_tbl'` = "gt_tbl" %in%
              class(gt_object))
  stopifnot(`There must be 2 to 4 labels` = (length(labels) %in%
                                               c(2:4)))
  stopifnot(`There must be 2 to 4 colors in the palette` = (length(palette) %in%
                                                              c(2:4)))
  stopifnot(`\`position\` must be one of 'stack' or 'fill'` = (position %in%
                                                                 c("stack", "fill")))
  var_sym <- rlang::enquo(column)
  var_bare <- rlang::as_label(var_sym)
  all_vals <- gt_index(gt_object, {
    {
      column
    }
  }) %>% lapply(X = ., FUN = sum, na.rm = TRUE) %>% unlist()
  if (length(all_vals) == 0) {
    return(gt_object)
  }
  total_rng <- max(all_vals, na.rm = TRUE)
  tab_out <- text_transform(
    gt_object,
    locations = cells_body({
      {
        column
      }
    }),
    fn = function(x) {
      bar_fx <- function(x_val) {
        if (x_val %in% c("NA", "NULL")) {
          return("<div></div>")
        }
        col_pal <- palette
        vals <- strsplit(x_val, split = ", ") %>% unlist() %>%
          as.double()
        n_val <- length(vals)
        stopifnot(`There must be 2 to 4 values` = (n_val %in%
                                                     c(2:4)))
        col_fill <- if (n_val == 2) {
          c(1, 2)
        }
        else {
          c(1:n_val)
        }
        df_in <- dplyr::tibble(x = vals,
                               y = rep(1, n_val),
                               fill = col_pal[col_fill])
        plot_out <-
          df_in %>% ggplot(aes(
            x = .data$x,
            y = factor(.data$y),
            fill = I(.data$fill),
            group = .data$y
          )) + geom_col(position = position,
                        color = "white",
                        size = 1) + geom_text(
                          aes(label = fmt_fn(x)),
                          hjust = 0.5,
                          size = 3,
                          family = "mono",
                          fontface = "bold",
                          position = if (position ==
                                         "fill") {
                            position_fill(vjust = 0.5)
                          }
                          else if (position == "stack") {
                            position_stack(vjust = 0.5)
                          },
                          color = "white"
                        ) + scale_x_continuous(expand = if (position ==
                                                            "stack") {
                          expansion(mult = c(0, 0.1))
                        }
                        else {
                          c(0, 0)
                        }, limits = if (position == "stack") {
                          c(0, total_rng)
                        }
                        else {
                          NULL
                        }) + scale_y_discrete(expand = c(0, 0)) + coord_cartesian(clip = "off") +
          theme_void() + theme(legend.position = "none",
                               plot.margin = margin(0, 0, 0, 0, "pt"))
        out_name <- file.path(tempfile(
          pattern = "file",
          tmpdir = tempdir(),
          fileext = ".svg"
        ))
        ggsave(
          out_name,
          plot = plot_out,
          dpi = 25.4,
          height = 5,
          width = width,
          units = "mm",
          device = "svg"
        )
        img_plot <- readLines(out_name) %>% paste0(collapse = "") %>%
          gt::html()
        on.exit(file.remove(out_name), add = TRUE)
        img_plot
      }
      tab_built <- lapply(X = x, FUN = bar_fx)
    }
  )
  # set up labels
  label_built <- if (max(all_vals) == 2) {
    lab_pal1 <- palette[1]
    lab_pal2 <- palette[2]
    lab1 <- labels[1]
    lab2 <- labels[2]
    glue::glue(
      "<span style='color:{lab_pal1}'><b>{lab1}</b></span>",
      "||",
      "<span style='color:{lab_pal2}'><b>{lab2}</b></span>"
    ) %>%
      gt::html()
  }
  else if (max(all_vals) == 3) {
    lab_pal1 <- palette[1]
    lab_pal2 <- palette[2]
    lab_pal3 <- palette[3]
    lab1 <- labels[1]
    lab2 <- labels[2]
    lab3 <- labels[3]
    glue::glue(
      "<span style='color:{lab_pal1}'><b>{lab1}</b></span>",
      "||",
      "<span style='color:{lab_pal2}'><b>{lab2}</b></span>",
      "||",
      "<span style='color:{lab_pal3}'><b>{lab3}</b></span>"
    ) %>%
      gt::html()
  }
  else {
    lab_pal1 <- palette[1]
    lab_pal2 <- palette[2]
    lab_pal3 <- palette[3]
    lab_pal4 <- palette[4]
    lab1 <- labels[1]
    lab2 <- labels[2]
    lab3 <- labels[3]
    lab4 <- labels[4]
    glue::glue(
      "<span style='color:{lab_pal1}'><b>{lab1}</b></span>",
      "||",
      "<span style='color:{lab_pal2}'><b>{lab2}</b></span>",
      "||",
      "<span style='color:{lab_pal3}'><b>{lab3}</b></span>",
      "||",
      "<span style='color:{lab_pal4}'><b>{lab4}</b></span>"
    ) %>%
      gt::html()
  }
  tab_out <- gt:::dt_boxhead_edit_column_label(data = tab_out,
                                               var = var_bare,
                                               column_label = label_built)
  suppressWarnings(tab_out)
}






#----------------------------------
# function for sparkline
#----------------------------------
gt_ggplot_sparkline <- function(table_data, plot_col, data_col, plot_fun, ...) {
  # save the data extract ahead of time
  # to be used in our anonymous function below
  data_in <- purrr::pluck(table_data, "_data", data_col)
  
  # colnames
  col_names = colnames(rbindlist(data_in))
  
  # interest variable
  var_interest = setdiff(col_names, "day_departure")
  
  # retrieve min max
  range_x = rbindlist(data_in)[, range(day_departure)]
  range_y = rbindlist(data_in)[, range(get(var_interest))]
  
  # draw plot
  text_transform(
    table_data,
    # note the use of {{}} here - this is tidy eval
    # that allows you to indicate specific columns
    locations = cells_body(columns = c({{ plot_col }})),
    fn = function(x) {
      # build the plot
      plot = lapply(data_in, function(x){
        # build the plot
        ggplot(x, aes(x=day_departure, y=get(var_interest))) +
          geom_path(size = 10) +
          coord_cartesian(xlim = range_x, ylim = range_y) +
          theme_void() + 
          theme(panel.grid=element_blank(), 
                panel.border=element_blank())
      })
      
      # draw for every row
      lapply(plot, ggplot_image, aspect_ratio = 4, height = 20)
    }
  )
}
