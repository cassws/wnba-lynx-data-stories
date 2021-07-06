library(WNBAballr)
library(nbastatR)
library(tidyverse)
library(stringr)

lay <- WNBAPlayerPerGameStats('/players/c/clarela01w.html')
lay_games <- c(1022100100,
               1022100095,
               1022100086,
               1022100082,
               1022100077,
               1022100072,
               1022100066,
               1022100060,
               1022100052,
               1022100050,
               1022100045,
               1022100037
)

away_games <- c(1022100100, 1022100095, 1022100082, 1022100077, 1022100072,
                1022100052)

# experiments
# test_out <- play_by_play(game_ids = c(1022100100), nest_data = F, return_message = T)
# colnames(test_out)

# get games since clarendon

lynx_with_lay <- play_by_play(game_ids = lay_games, nest_data = F, return_message = T)

# make sure to identify lynx in play by play

lynx_plays <- lynx_with_lay %>%
  mutate(lynxPlay = ifelse(idGame %in% away_games, descriptionPlayVisitor, descriptionPlayHome)) %>%
  filter(!is.na(lynxPlay)) %>%
  select(lynxPlay, idGame)

lynx_plays

lynx_assists <- lynx_plays %>%
  filter(grepl("\\d+\\sAST", lynxPlay))

lynx_assists

# prepare for network visualization
# we need nodes and (directed) edges: nodes are players
# & edges are assists (FROM assist TO player)
# more documentation to add!

lynx_network_ready <- lynx_assists %>%
  mutate(player = word(lynxPlay, 1)) %>%
  mutate(assist = str_sub(word(lynxPlay, -3), 2, -1)) %>%
  mutate(points = ifelse(grepl("3PT", lynxPlay), "3", "2"))

lynx_network_ready %>%
  select(assist, player)



assisters <- lynx_network_ready %>%
  distinct(assist) %>%
  rename(node = assist)

shooters <- lynx_network_ready %>%
  distinct(player) %>%
  rename(node = player)

library(igraph)

nodes <- bind_rows(assisters, shooters) %>%
  distinct(node) %>%
  rename(id = node) %>%
  mutate(label = id)

edges <- lynx_network_ready %>%
  select(assist, player) %>%
  rename(from = assist) %>%
  rename(to = player) %>%
  group_by(from, to) %>%
  summarise(value = n()) %>%
  mutate(arrows = "to")

edges %>%
  filter(value > 9 ) %>%
  ggplot() +
  geom_bar(aes(x=paste0(from, " -> ", to), y=value), stat="identity")
# graph_from_data_frame(nodes, directed=TRUE, vertices=edges)

library(visNetwork)
visNetwork(nodes = nodes, edges = edges, width = "100%", height = 700, footer = list(text="Lynx assists since acquiring Layshia Clarendon")) %>%
  visPhysics(solver = "barnesHut", barnesHut = list(centralGravity = 0.2, avoidOverlap = 0.3)) %>%
  visEdges(shadow = FALSE,
           widthConstraint = 4,
           arrows =list(to = list(enabled = TRUE, scaleFactor = 0.5)),
           color = list(color = "lightblue", opacity = 0.8, highlight = "red")) %>%
 # visIgraphLayout(layout = "layout_in_circle") %>%
  visNodes(size = 5) %>%
  visOptions(highlightNearest = list(enabled = T, hover = T), 
             nodesIdSelection = T)


# https://stats.wnba.com/game/1022100100 , away