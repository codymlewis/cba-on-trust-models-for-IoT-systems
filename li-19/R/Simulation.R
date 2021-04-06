#' @include Adversaries.R
#' @include ServiceProvider.R
#' @include Field.R
#' @include Device.R
#' @include Observer.R
NULL

#' Run the simulation in a console interface
#'
#' Simulate the trust model and the mobile network and iterate for an amount
#' of time
#' @keywords trust model simulate simulation run
#' @export run_simulation

run_simulation <- function(total_time,
                           map_filename = system.file(
                                   "extdata", "map.csv",
                                   package = "li19trustmodel"
                           ),
                           config = system.file(
                                   "extdata", "params.json",
                                   package = "li19trustmodel"
                           ),
                           write_plots = TRUE) {
        return(run_sim_part(total_time, map_filename, rjson::fromJSON(file = config), write_plots))
}


#' Run a batch of simulations in a console interface
#'
#' Simulate the trust model and the mobile network and iterate for an amount
#' of time for differing amounts of adversaries
#' @keywords trust model simulate simulation run
#' @export batch_simulation

batch_simulation <- function(total_time,
                             map_filename = system.file(
                                     "extdata", "map.csv",
                                     package = "li19trustmodel"
                             ),
                             config = system.file(
                                     "extdata", "params.json",
                                     package = "li19trustmodel"
                             ),
                             num_adversaries = c(0, 2, 5, 8, 10),
                             adversary_types = c("BadMouther", "ContextSetter"),
                             colours = c("blue", "red", "green", "orange", "purple")) {
        dir.create("images/plots", recursive = TRUE, showWarning = FALSE)
        config <- rjson::fromJSON(file = config)
        for (adv_type in adversary_types) {
                config$adversary_type <- adv_type
                cat(sprintf("Running simulations with adversaries of %s type\n\n", adv_type))
                data_list <- lapply(
                        num_adversaries,
                        function(i) {
                                config$number_adversaries <- i
                                cat(sprintf("Running simulations with %d adversaries...\n", i))
                                result <- run_sim_part(total_time, map_filename, config, FALSE)
                                cat(sprintf("Converged trust estimate: %f\n", mean(tail(result$estimated_trusts, 100))))
                                return(result)
                        }
                )
                names(data_list) <- sprintf("%d Adversaries", num_adversaries)
                data <- reshape2::melt(data_list, id.vars = "transactions")
                data$L1 <- factor(
                        data$L1,
                        levels = stringr::str_sort(levels(as.factor(data$L1)), numeric = TRUE)
                )
                cat("Creating plots...\n")
                ggplot2::ggplot(
                        data = data,
                        ggplot2::aes(x = transactions, y = value, colour = as.factor(L1))
                ) +
                        ggplot2::geom_line() +
                        ggplot2::scale_colour_manual(values = colours) +
                        ggplot2::labs(
                                title = "Estimated Trusts of the Observer",
                                x = "Time",
                                y = "Estimated Trust",
                                colour = NULL
                        ) +
                        ggplot2::scale_y_continuous(limits = c(-1.1, 1.1)) +
                        ggplot2::theme(legend.position = "bottom")
                filename <- sprintf("images/plots/%s-estimated_trusts.png", adv_type)
                ggplot2::ggsave(file = filename, width = 7, height = 7, dpi = 320, type = "cairo")
                cat(sprintf("Saved plot as %s\n", filename))
        }
}


run_sim_part <- function(total_time, map_filename, config, write_plots) {
        params$configure(config)
        map_and_devices <- create_map_and_devices(map_filename)
        dir.create("images/maps", recursive = TRUE, showWarning = FALSE)
        img <- write_map(map_and_devices$map)
        cat("Performing transactions...\n")
        while (params$time_now <= total_time) {
                set_trusts(map_and_devices$devices)
                movements <- transact_and_move(map_and_devices$devices)
                img <- update_map(params$time_now, movements[[1]], movements[[2]], img, map_and_devices$map)
                cat_progress(
                        params$time_now,
                        total_time,
                        prefix = sprintf("Time %d of %d", params$time_now, total_time)
                )
                params$increment_time()
        }
        cat("Done.\n\n")
        if (write_plots) {
                cat("Plotting estimated trusts...\n")
                dir.create("images/plots", showWarning = FALSE)
                for (i in 1:params$number_nodes) {
                        plot_estimated_trust(
                                i,
                                map_and_devices$devices
                        )
                        filename <- sprintf("images/plots/device-%d-estimated-trust.png", i)
                        ggplot2::ggsave(file = filename, width = 7, height = 7, dpi = 320, type = "cairo")
                        cat_progress(
                                i,
                                params$number_nodes,
                                prefix = sprintf("Device %d of %d", i, params$number_nodes),
                                postfix = sprintf("Saved to %s", filename)
                        )
                }
        }
        return(
                data.frame(
                        transactions = seq_len(
                                length(map_and_devices$devices[[params$number_nodes]]$estimated_trusts)
                        ),
                        estimated_trusts = map_and_devices$devices[[params$number_nodes]]$estimated_trusts
                )
        )
}


#' Run the simulation in a graphical interface
#'
#' Simulate the trust model and the mobile network and iterate
#' @keywords trust model simulate simulation run
#' @export run_gui

run_gui <- function(map_filename = system.file("extdata", "map.csv", package = "li19trustmodel"),
                    config = system.file(
                            "extdata", "params.json",
                            package = "li19trustmodel"
                    )) {
        params$configure(rjson::fromJSON(file = config))
        map_and_devices <- create_map_and_devices(map_filename)
        dir.create("images/maps", recursive = TRUE, showWarning = FALSE)
        dir.create("images/plots", recursive = TRUE, showWarning = FALSE)
        img <- write_map(map_and_devices$map)
        map_filename <- sprintf("images/maps/map-%d.png", params$time_now)
        cat("Performing transactions...\n")
        gui_objects <- build_gui(map_and_devices)
        repeat {
                set_trusts(map_and_devices$devices)
                movements <- transact_and_move(map_and_devices$devices)
                img <- update_map(
                        params$time_now,
                        movements[[1]],
                        movements[[2]],
                        img,
                        map_and_devices$map
                )
                gui_objects <- update_gui(gui_objects, map_and_devices, movements, img)
                params$increment_time()
        }
}


build_gui <- function(map_and_devices) {
        tt <- tcltk::tktoplevel()
        tcltk::tktitle(tt) <- "Li 2019 Trust Model"
        tp <- gui_add_frame(tt)
        chosen_node_cb <- gui_add_node_chooser(tp)
        chosen_node <- find_chosen_node(chosen_node_cb, map_and_devices)
        cn <- map_and_devices$devices[[chosen_node]]
        gui_add_close_button(tp, map_and_devices, chosen_node)
        return(
                list(
                        timelabel = gui_add_time(tp),
                        maplabel = gui_add_map(tt),
                        trustlabel = gui_add_trust(tt, map_and_devices),
                        chosen_node_cb = chosen_node_cb,
                        chosen_node = chosen_node,
                        contextvals_label = gui_add_context(tp, cn),
                        reps_label = gui_add_reputations(tp, map_and_devices, cn),
                        netlabel = gui_add_network(tt, cn),
                        old_chosen_node = chosen_node
                )
        )
}


gui_add_map <- function(tt) {
        tcltk::tcl(
                "image",
                "create",
                "photo",
                "map",
                file = sprintf("images/maps/map-%d.png", params$time_now)
        )
        maplabel <- tcltk2::tk2label(tt, image = "map", compound = "image")
        tcltk::tkgrid(maplabel, row = "0", column = "0")
        return(maplabel)
}


gui_add_trust <- function(tt, map_and_devices) {
        filename <- tempfile(fileext = ".png")
        Cairo::CairoPNG(filename = filename, width = params$img_width, height = params$img_height)
        print(
                plot_estimated_trust(
                        length(map_and_devices$devices),
                        map_and_devices$devices
                )
        )
        dev.off()
        tcltk::tcl("image", "create", "photo", "trustest", file = filename)
        trustlabel <- tcltk2::tk2label(tt, image = "trustest", compound = "image")
        tcltk::tkgrid(trustlabel, row = "0", column = "1")
        return(trustlabel)
}


gui_add_frame <- function(tt) {
        tp <- tcltk2::tk2frame(tt)
        tcltk::tkgrid(tp, row = "1", column = "1")
        return(tp)
}

gui_add_time <- function(tt) {
        timelabel <- tcltk2::tk2label(tt, text = sprintf("Current time: %d", params$time_now))
        tcltk::tkgrid(timelabel, row = "0", column = "1")
        return(timelabel)
}


gui_add_node_chooser <- function(tt) {
        tcltk::tkgrid(tcltk2::tk2label(tt, text = "Chosen node: "), row = "1", column = "0")
        chosen_node_cb <- tcltk::ttkcombobox(
                tt,
                textvariable = paste(),
                values = seq_len(params$number_nodes)
        )
        tcltk::tkgrid(chosen_node_cb, row = "1", column = "1")
        return(chosen_node_cb)
}


update_gui <- function(gui_objects, map_and_devices, movements, img) {
        gui_update_map()
        gui_objects$chosen_node <- find_chosen_node(gui_objects$chosen_node_cb, map_and_devices)
        gui_update_trust(gui_objects$chosen_node, map_and_devices)
        cn <- map_and_devices$devices[[gui_objects$chosen_node]]
        if (gui_objects$chosen_node != gui_objects$old_chosen_node) {
                gui_update_network(cn)
                gui_objects$old_chosen_node <- gui_objects$chosen_node
        }
        gui_update_time(gui_objects)
        gui_update_context(gui_objects, cn)
        gui_update_reputation(gui_objects, cn, map_and_devices)
        return(gui_objects)
}


gui_update_map <- function() {
        tcltk::tcl(
                "image",
                "create",
                "photo",
                "map",
                file = sprintf("images/maps/map-%d.png", params$time_now)
        )
}


gui_update_trust <- function(chosen_node, map_and_devices) {
        filename <- tempfile(fileext = ".png")
        Cairo::CairoPNG(filename = filename, width = params$img_width, height = params$img_height)
        print(
                plot_estimated_trust(
                        chosen_node,
                        map_and_devices$devices
                )
        )
        dev.off()
        tcltk::tcl("image", "create", "photo", "trustest", file = filename)
}


gui_update_network <- function(cn) {
        filename <- tempfile(fileext = ".png")
        Cairo::CairoPNG(
                filename = filename,
                width = params$img_width,
                height = params$img_height
        )
        plot_network(cn)
        dev.off()
        tcltk::tcl("image", "create", "photo", "network", file = filename)
}


gui_update_time <- function(gui_objects) {
        tcltk::tkconfigure(
                gui_objects$timelabel,
                text = sprintf("Current time: %d", params$time_now)
        )
}


gui_update_context <- function(gui_objects, cn) {
        tcltk::tkconfigure(
                gui_objects$contextvals_label,
                text = paste(
                        sprintf(
                                "%s:\t%f",
                                c("Capability", "Distance", "Velocity"),
                                cn$get_target_context()[2:4]
                        ),
                        collapse = "\n"
                )
        )
}


gui_update_reputation <- function(gui_objects, cn, map_and_devices) {
        tcltk::tkconfigure(
                gui_objects$reps_label,
                text = paste(
                        sprintf(
                                "Node %d:\t%f",
                                seq_len(length(map_and_devices$devices)),
                                cn$reputations
                        ),
                        collapse = "\n"
                )
        )
}


find_chosen_node <- function(chosen_node_cb, map_and_devices) {
        chosen_node <- as.integer(tcltk::tkget(chosen_node_cb))
        return(
                `if`(
                        length(chosen_node) == 0 ||
                                is.na(chosen_node) ||
                                chosen_node > length(map_and_devices$devices),
                        length(map_and_devices$devices),
                        chosen_node
                )
        )
}


gui_add_context <- function(tt, cn) {
        tcltk::tkgrid(tcltk2::tk2label(tt, text = "Contexts:"), row = "2", column = "0")
        contextvals_label <- tcltk2::tk2label(
                tt,
                text = paste(
                        sprintf(
                                "%s:\t%f",
                                c("Capability", "Distance", "Velocity"),
                                cn$get_target_context()[2:4]
                        ),
                        collapse = "\n"
                )
        )
        tcltk::tkgrid(contextvals_label, row = "2", column = "1")
        return(contextvals_label)
}


gui_add_reputations <- function(tt, map_and_devices, cn) {
        tcltk::tkgrid(tcltk2::tk2label(tt, text = "Reputations:"), row = "3", column = "0")
        reps_label <- tcltk2::tk2label(
                tt,
                text = paste(
                        sprintf(
                                "Node %d:\t%f",
                                1:length(map_and_devices$devices),
                                cn$reputations
                        ),
                        collapse = "\n"
                )
        )
        tcltk::tkgrid(reps_label, row = "3", column = "1")
        return(reps_label)
}


gui_add_network <- function(tt, cn) {
        filename <- tempfile(fileext = ".png")
        Cairo::CairoPNG(filename = filename, width = params$img_width, height = params$img_height)
        plot_network(cn)
        dev.off()
        tcltk::tcl("image", "create", "photo", "network", file = filename)
        netlabel <- tcltk2::tk2label(tt, image = "network", compound = "image")
        tcltk::tkgrid(netlabel, row = "1", column = "0")
        return(netlabel)
}


gui_add_close_button <- function(tt, map_and_devices, chosen_node) {
        tcltk::tkgrid(tcltk2::tk2label(tt, text = " "), row = "4", column = "1")
        tcltk::tkgrid(
                tcltk2::tk2button(
                        tt,
                        text = "Save and Exit",
                        command = function() {
                                plot_estimated_trust(
                                        chosen_node,
                                        map_and_devices$devices
                                )
                                filename <- sprintf(
                                        "images/plots/device-%d-estimated-trust.png",
                                        chosen_node
                                )
                                ggplot2::ggsave(file = filename, width = 7, height = 7, dpi = 320, type = "cairo")
                                cat(sprintf("Saved estimated trust plot to %s\n", filename))
                                cat("Bye.\n")
                                tcltk::tkdestroy(tt)
                                quit("no")
                        }
                ),
                row = "5",
                column = "1"
        )
}


write_map <- function(map, save = TRUE) {
        cat("Creating map image...\n")
        red <- matrix(0, nrow = params$img_height, ncol = params$img_width)
        green <- matrix(0, nrow = params$img_height, ncol = params$img_width)
        blue <- matrix(0, nrow = params$img_height, ncol = params$img_width)
        img <- array(
                c(red, green, blue),
                dim = c(params$img_height, params$img_width, 3)
        )
        width_factor <- ceiling(params$img_width / params$map_width)
        height_factor <- ceiling(params$img_height / params$map_height)
        for (i in 1:params$map_height) {
                for (j in 1:params$map_width) {
                        cur_tile <- map$get_tile(c(i, j))[[1]]
                        for (k in 1:height_factor) {
                                for (l in 1:width_factor) {
                                        img[
                                                (i - 1) * height_factor + k,
                                                (j - 1) * width_factor + l,
                                        ] <- draw_map(cur_tile)
                                }
                        }
                }
                cat_progress(
                        i,
                        params$map_height,
                        prefix = sprintf("Row %d of %d", i, params$map_height)
                )
        }
        if (save) {
                filename <- sprintf("images/maps/map-%d.png", params$time_now)
                png::writePNG(img, filename)
                cat(sprintf("Written %s\n", filename))
        }
        return(img)
}


update_map <- function(time, old_locs, new_locs, img, map, save = TRUE) {
        img <- update_map_locs(old_locs, img, map)
        img <- update_map_locs(new_locs, img, map)
        if (save) {
                filename <- sprintf("images/maps/map-%d.png", time)
                png::writePNG(img, filename)
        }
        return(img)
}


update_map_locs <- function(locs, img, map) {
        width_factor <- ceiling(params$img_width / params$map_width)
        height_factor <- ceiling(params$img_height / params$map_height)
        for (loc in locs) {
                cur_tile <- map$get_tile(loc)[[1]]
                for (i in 1:height_factor) {
                        for (j in 1:width_factor) {
                                img[
                                        (loc[[1]] - 1) * height_factor + i, (loc[[2]] - 1)
                                        * width_factor + j,
                                ] <- draw_map(cur_tile)
                        }
                }
        }
        return(img)
}


draw_map <- function(cur_tile) {
        result <- c(0, 0, 0)
        if (cur_tile$terrain == WATER) {
                result <- c(0.063, 0.612, 0.820)
        } else {
                result <- c(0.549, 0.761, 0.376)
        }
        if (cur_tile$signal_edge) {
                result <- sapply(
                        result,
                        function(i) {
                                max(0, result[[i]] - 0.1)
                        }
                )
        }
        if (length(cur_tile$service_provider)) {
                result <- c(0, 1, 0)
        }
        if (length(cur_tile$base_station)) {
                result <- c(0.2, 0.2, 0.2)
        }
        if (cur_tile$has_devices()) {
                dev_class <- class(cur_tile$get_first_dev())[[1]]
                if (grepl("Device", dev_class)) {
                        result <- c(0, 0, 1)
                } else if (grepl("Observer", dev_class)) {
                        result <- c(0.4, 0.4, 0.4)
                } else {
                        result <- c(1, 0, 0)
                }
        }
        return(result)
}


create_map_and_devices <- function(map_filename) {
        sp <- ServiceProvider$new()
        map <- Field$new(read.csv(map_filename, header = F), T)
        map$add_service_provider(sp)
        return(list(map = map, devices = create_devices(sp, map)))
}


new_map_and_devices <- function(map_filename, devices) {
        sp <- ServiceProvider$new()
        map <- Field$new(read.csv(map_filename, header = F), T)
        map$add_service_provider(sp)
        return(list(map = map, devices = copy_devices(sp, map, devices)))
}


create_devices <- function(sp, map) {
        cat("Creating devices...\n")
        devices <- create_adversaries(sp, map)
        devices <- create_honest_nodes(sp, map, devices)
        # devices <- create_nodes(sp, map)
        assign_contacts(devices)
        i <- length(devices) + 1
        devices[[i]] <- create_observer(i, sp, map)
        assign_observer_contacts(devices)
        return(devices)
}


copy_devices <- function(sp, map, devices) {
        cat("Copying devices...")
        return(copy_nodes(sp, map, devices))
}


create_adversaries <- function(sp, map) {
        return(
                lapply(
                        seq_len(params$number_adversaries),
                        function(i) {
                                cat_progress(
                                        i,
                                        params$number_nodes,
                                        prefix = sprintf("Device %d of %d", i, params$number_nodes)
                                )
                                return(params$adversary_type$new(i, sp, map))
                        }
                )
        )
}


make_adversaries <- function(sp, map, devices) {
        for (i in seq_len(params$number_adversaries)) {
                cat_progress(
                        i,
                        params$number_adversaries,
                        prefix = sprintf("Device %d of %d", i, params$number_adversaries)
                )
                return(params$adversary_type$new(i, sp, map, copy = devices[[i]]))
        }
        return(devices)
}


create_honest_nodes <- function(sp, map, devices) {
        for (i in seq_len(params$number_good_nodes)) {
                dev_id <- params$number_adversaries + i
                cat_progress(
                        dev_id,
                        params$number_nodes,
                        prefix = sprintf("Device %d of %d", i, params$number_nodes)
                )
                devices[[dev_id]] <- Device$new(dev_id, sp, map)
        }
        return(devices)
}

create_nodes <- function(sp, map) {
        return(
               lapply(
                      seq_len(params$number_nodes),
                      function(i) {
                              cat_progress(
                                           dev_id,
                                           params$number_nodes,
                                           prefix = sprintf("Device %d of %d", i, params$number_nodes)
                              )
                              return(Device$new(i, sp, map))
                      }
               )
        )
}


create_observer <- function(i, sp, map) {
        obs <- Observer$new(i, sp, map)
        cat_progress(
                i,
                params$number_nodes,
                prefix = sprintf("Device %d of %d", i, params$number_nodes)
        )
        return(obs)
}


assign_contacts <- function(devices) {
        lapply(
                seq_len(length(devices)),
                function(i) {
                        if (i <= params$number_adversaries) {
                                devices[[i]]$add_contact(
                                        setdiff(1:length(devices), i),
                                        devices
                                )
                        } else {
                                devices[[i]]$add_contact(
                                        sample(
                                                setdiff(1:length(devices), i),
                                                min(params$contacts_per_node, params$number_nodes - 2)
                                        ),
                                        devices
                                )
                        }
                }
        )
        if (params$number_adversaries + 1 < params$number_nodes) {
                i <- params$number_adversaries + 1
                devices[[i]]$add_contact(setdiff(1:length(devices), i), devices)
        }
}


assign_observer_contacts <- function(devices) {
        adv_ids <- `if`(
                params$number_adversaries == 0,
                NULL,
                1:params$number_adversaries
        )
        num_norm_con <- params$number_observer_contacts - params$number_adversaries - 1
        possible_contacts <- (params$number_adversaries + 2):
        (params$number_adversaries + params$number_good_nodes)
        devices[[length(devices)]]$add_contact(
                c(
                        `if`(
                                length(possible_contacts) > 1,
                                sample(
                                        possible_contacts,
                                        `if`(num_norm_con < 0, 0, num_norm_con)
                                ),
                                possible_contacts
                        ),
                        `if`(num_norm_con > 0, params$number_adversaries + 1, NULL),
                        adv_ids
                ),
                devices
        )
        cat(
                sprintf(
                        "The observer has %d contacts where %d %s\n",
                        length(devices[[length(devices)]]$contacts),
                        length(adv_ids),
                        `if`(length(adv_ids) == 1, "is an adversary", "are adversaries")
                )
        )
}


set_trusts <- function(devices) {
        for (device in devices) {
                device$set_trusts()
        }
}


transact_and_move <- function(devices) {
        old_locs <- list()
        new_locs <- list()
        for (device in devices) {
                old_locs[[device$id]] <- device$location
                if (device$has_signal()) {
                        amount_transactions <- params$min_trans:round(
                                runif(1, min = params$min_trans, max = params$max_trans)
                        )
                        for (i in setdiff(amount_transactions, 0)) {
                                device$transaction(devices)
                        }
                        if (length(setdiff(amount_transactions, 0)) >= 1) {
                                device$send_rec(devices)
                        }
                } else {
                        device$transactions(devices, can_transact = FALSE)
                }
                device$move()
                new_locs[[device$id]] <- device$location
        }
        for (device in devices) {
                device$performance_updates()
                device$combine_reps()
        }
        return(list(old_locs, new_locs))
}


plot_estimated_trust <- function(dev_id, devices) {
        data <- data.frame(
                transactions = seq_len(length(devices[[dev_id]]$estimated_trusts)),
                estimated_trusts = devices[[dev_id]]$estimated_trusts
        )
        plt <- ggplot2::ggplot(data = data, ggplot2::aes(x = transactions, y = estimated_trusts)) +
                ggplot2::labs(
                        title = `if`(
                                dev_id == params$number_nodes,
                                "Estimated Trusts of Device the Observer",
                                sprintf("Estimated Trusts of Device %d", dev_id)
                        ),
                        x = "Time",
                        y = "Estimated Trust",
                        colour = NULL
                ) +
                ggplot2::scale_y_continuous(limits = c(-1.1, 1.1))
        line_colour <- `if`(dev_id <= params$number_adversaries, "red", "blue")
        return(
                `if`(
                        length(devices[[dev_id]]$estimated_trusts) > 1,
                        plt + ggplot2::geom_line(colour = line_colour),
                        plt + ggplot2::geom_point(colour = line_colour)
                )
        )
}


csv_estimated_trust <- function(dev_id, devices) {
        write.csv(
                data.frame(
                        transactions = seq_len(length(devices[[dev_id]]$estimated_trusts)),
                        estimated_trusts = devices[[dev_id]]$estimated_trusts
                ),
                file = sprintf("%d-estimated-trusts.csv", dev_id),
                row.names = FALSE
        )
}


plot_network <- function(dev) {
        g <- igraph::make_empty_graph()
        g <- igraph::add_vertices(g, params$number_adversaries, color = "red", label.color = "white")
        g <- igraph::add_vertices(g, params$number_good_nodes, color = "blue", label.color = "white")
        g <- igraph::add_vertices(g, 1, color = "gray")
        for (i in dev$contacts) {
                g <- igraph::add_edges(g, c(dev$id, i))
        }
        plot(igraph::as.undirected(g, "collapse"))
}
