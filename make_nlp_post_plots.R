library(dplyr)
library(ggplot2)
library(stringr)
library(tidytext)
library(formattable)

main_chars_list <- list('friends' = c("chandler", "joey", "monica", "phoebe", "rachel", "ross"),
                        'frasier' = c("daphne", "frasier", "martin", "niles", "roz"),
                        'wg' = c("grace", "jack", "karen", "will"),
                        'office' = c("dwight", "jim", "michael", "pam", "andy", "darryl", "erin", "ryan"),
                        'seinfeld' = c("elaine", "george", "jerry", "kramer"),
                        'marriedwc' = c("al", "kelly", "marcy", "peggy", "steve"),
                        'scrubs' = c("carla", "elliot", "turk", "j.d.", "dr. cox", "dr. kelso"))

#Select colour scheme by show
colour_dict <- c('friends'='#66c2a5',
                 'frasier'='#fc8d62',
                 'wg'='#8da0cb',
                 'willandgrace'='#8da0cb',
                 'office'='#e78ac3',
                 'seinfeld'='#a6d854',
                 'marriedwc'='#ffd92f',
                 'scrubs'='#e5c494')

process_transcripts <- function(transcript_lines) {
    ep_df <- data.frame(stringsAsFactors = FALSE)
    for (l in transcript_lines) {
        if (grepl('^[\\w\\s\\.]*:', l, perl=TRUE) & 
            !grepl('html|http', l, ignore.case=TRUE) &
            !grepl('align=', l, ignore.case=TRUE)) {
            char <- tolower(str_match(l, '([\\w\\s\\.]*):')[,2])
            #remove speaker tag
            l <- str_remove(l, '[\\w\\s\\.]*: ?')
            #remove stage directions
            l <- str_remove_all(l, '\\[.*?\\]')
            l <- str_remove_all(l, '\\(.*?\\)')
            #remove those &gt;s
            l <- str_remove_all(l, '&gt;')
            l <- str_remove_all(l, '&lt;')
            l <- str_replace_all(l, '&egrave;', 'e')
            l <- str_replace_all(l, '&agrave;', 'a')
            l <- str_replace_all(l, '&eacute;', 'e')
            l <- str_replace_all(l, '&aacute;', 'a')
            l <- str_remove_all(l, '<br />')
            #
            ep_df <- rbind(ep_df, data.frame(char=char, text=l, stringsAsFactors=FALSE))
        }
    }
    ep_df
}

#1) Something with char-ep sentiments----

ep_char_sentiments <- lapply(c('friends','frasier','wg','office','seinfeld','marriedwc','scrubs'), 
                             function(s) data.frame(show=s, 
                                                    read.csv(sprintf('seas_ep_char_sentiments_%s.csv',s), stringsAsFactors = FALSE),
                                                    stringsAsFactors=FALSE)) %>%
    bind_rows()
ep_char_sentiments$X <- NULL
ep_char_sentiments$show[ep_char_sentiments$show=='will_and_grace'] <- 'willandgrace'
ep_char_sentiments$show[ep_char_sentiments$show=='the_office'] <- 'office'
ep_char_sentiments$show[ep_char_sentiments$show=='married_with_children'] <- 'marriedwc'

#Show level ---
ep_char_sentiments %>% group_by(show) %>% 
    summarise(n_lines = sum(n_lines), mean_sent = mean(sent)) %>%
    arrange(-mean_sent)
#Overall, Friends most pos, seinfeld least by a distance

ep_char_sentiments %>% group_by(show, season) %>% 
    summarise(n_lines = n(), mean_sent = mean(sent)) %>% 
    filter(n_lines > 100) %>%   #skip seinfeld s0,1, office s1
    ggplot(aes(season, mean_sent, colour=show)) + geom_point() + geom_path(aes(group=show)) +
    scale_colour_manual(values = colour_dict) +
    theme(legend.position='bottom') 
#Quite noisy but may be plausible
#Frasier jumps up a lot in s8 - N&D get together
#Seinfeld notably below the rest
ep_char_sentiments %>% group_by(show, season) %>% 
    summarise(n_lines = n(), mean_sent = mean(sent)) %>% 
    filter(n_lines > 100) %>%   #skip seinfeld s0,1, office s1
    summarise(max_sent = max(mean_sent), min_sent = min(mean_sent), mean_sent = mean(mean_sent)) %>%
    mutate(show = fct_reorder(show, mean_sent)) %>%
    ggplot(aes(x=show, colour=show)) + geom_pointrange(aes(ymin=min_sent, y =mean_sent, ymax=max_sent), size=2) +
    guides(colour='none') +
    scale_colour_manual(values = colour_dict) +
    theme_linedraw() + 
    labs(x='', y='Season mean sentiment score')


#Character level  
#Filter out some characters
ep_char_sentiments_main <- subset(ep_char_sentiments, !(char %in% c('man','woman','tv')))
ep_char_sentiments_main <- subset(ep_char_sentiments_main, !(char %in% c('reset to','dissolve to')))
#Keep characters in 30+ episodes
ep_char_sentiments_main <- ep_char_sentiments_main %>% group_by(show, char) %>% 
    filter(n() >= 30, sum(n_lines) > 500) %>% ungroup()

#make sure to group by show and char to avoid minor characters
ep_char_sentiments_main %>% group_by(show, char) %>% summarise(n_lines = sum(n_lines), mean_sent = mean(sent)) %>%
    filter(n_lines > 1000) %>% arrange(mean_sent) %>% head(6)
#Angela most negative - makes sense; then Darryl, Jerry, Phyllis, Elaine, George, Oscar, Dwight - also possible
#  Stanley is even more negative; mostly Office, Sein, MWC in top 15
#  Scrubs comes in with turk, elliot, carla in most negative 6
ep_char_sentiments_main %>% group_by(char) %>% summarise(n_lines = sum(n_lines), mean_sent = mean(sent)) %>%
    filter(n_lines > 1000) %>% arrange(-mean_sent) %>% head(6)
#Michael, Frasier, Rachel, Jim, Phoebe, Jack most positive

ep_char_sentiments_main %>% group_by(show, char) %>% 
    summarise(mean_sent = weighted.mean(sent, n_lines),
              n_lines = sum(n_lines)) %>%
    filter(n_lines > 500) %>%
    ggplot() + geom_text(aes(n_lines, mean_sent, label=char, colour=show))
#Better to plot +ve vs -ve sent - see below

#Or show all episodes as sentiment distributions - not very informative. Too many characters to show at once
#Can split by show and distribution by episode:
ep_char_sentiments_main %>% 
    mutate(show = ifelse(show=='wg', 'willandgrace', show)) %>%
    group_by(show, season, ep) %>% summarise(sent = weighted.mean(sent, n_lines)) %>%
    group_by(show) %>% mutate(show_mean_sent = mean(sent)) %>% ungroup() %>%
    mutate(show = fct_reorder(show, show_mean_sent)) %>%
    ggplot() + geom_density_ridges(aes(sent, show, fill=show), alpha=1.0) +#, jittered_points=TRUE, position='raincloud') +
    scale_fill_manual(values = colour_dict) +
    scale_y_discrete(expand = expand_scale(c(0,0))) +#mult = c(0.01, .7))) +
    coord_cartesian(clip='off') +
    theme_minimal() +
    guides(fill='none') + labs(x='Episode sentiment score', y='', title='Show language sentiment from VADER')
ggsave('post2_show_ep_sentiments_vader_ridges.png', width=6, height=4)

#Use tidytext to see other sentiment categories
sentiments_nrc <- textdata::lexicon_nrc(dir = '/media/shared_storage/data/text_data/R_textdata_lexicons/') #download once
#Need to process the text again here

get_ep_sent_by_char <- function(filename) {
    tmp <- readLines(filename)
    ep_df <- process_transcripts(tmp)
    
    ep_df %>% group_by(char) %>% mutate(n_lines = n()) %>% ungroup() %>%
        unnest_tokens(output='word', input='text', token='words') %>% 
        left_join(textdata::lexicon_afinn(), by='word') %>% 
        rename(afinn_value = value) %>%
        left_join(subset(sentiments_nrc, sentiment=='sadness'), by='word') %>%
        mutate(nrc_sadness = ifelse(!is.na(sentiment), 1, 0)) %>% select(-sentiment) %>%
        left_join(subset(sentiments_nrc, sentiment=='joy'), by='word') %>%
        mutate(nrc_joy = ifelse(!is.na(sentiment), 1, 0)) %>% select(-sentiment) %>%
        left_join(subset(sentiments_nrc, sentiment=='disgust'), by='word') %>%
        mutate(nrc_disgust = ifelse(!is.na(sentiment), 1, 0)) %>% select(-sentiment) %>%
        left_join(subset(sentiments_nrc, sentiment=='negative'), by='word') %>%
        mutate(nrc_negative = ifelse(!is.na(sentiment), 1, 0)) %>% select(-sentiment) %>%
        left_join(subset(sentiments_nrc, sentiment=='positive'), by='word') %>%
        mutate(nrc_positive = ifelse(!is.na(sentiment), 1, 0)) %>% select(-sentiment) %>%
        group_by(char) %>% summarise(n_words = n(), 
                                     n_lines = first(n_lines),
                                     sent_afinn = mean(afinn_value, na.rm=TRUE),
                                     frac_nrc_sadness = mean(nrc_sadness),
                                     frac_nrc_joy = mean(nrc_joy),
                                     frac_nrc_disgust = mean(nrc_disgust),
                                     frac_nrc_negative = mean(nrc_negative),
                                     frac_nrc_positive = mean(nrc_positive))
}
#warnings are due to \x24 etc characters in text

friends_filelist <- Sys.glob('./friends_transcripts/refmt_*.txt')
res <- data.frame()
for (f in friends_filelist) {
    season <- substr(f, nchar(f)-7, nchar(f)-6)
    ep <- substr(f, nchar(f)-5, nchar(f)-4)
    res <- rbind(res, data.frame(season = as.integer(season), 
                                 ep = as.integer(ep),
                                 get_ep_sent_by_char(f)))
}
res %>% group_by(char) %>% summarise(n_eps = n(), tot_lines = sum(n_lines), 
                                     mean_sent_afinn = mean(sent_afinn, na.rm=T),
                                     frac_nrc_joy = mean(frac_nrc_joy),
                                     frac_nrc_sadness = mean(frac_nrc_sadness),
                                     frac_nrc_disgust = mean(frac_nrc_disgust),
                                     frac_nrc_negative = mean(frac_nrc_negative),
                                     frac_nrc_positive = mean(frac_nrc_positive)) %>%
    filter(tot_lines > 1000) %>% arrange(-mean_sent_afinn)

#See if afinn results are somewhat similar to Vader, where order is Ra,Ph,Ro,Jo,Ch,Mo
ep_char_sentiments %>% filter(show=='friends') %>% group_by(show, char) %>% 
    summarise(n_lines = sum(n_lines), mean_sent_vader = mean(sent)) %>% filter(n_lines > 1000)
#afinn order is Ra,Ro,Mo,Ph,Jo,Ch
#nrc pos-neg order is Ra,Ph,Mo,Ro,Ch,Jo
#nrc and vader agree quite well

#plot nrc neg vs pos
res %>% group_by(char) %>% summarise(n_eps = n(), tot_lines = sum(n_lines), 
                                     frac_nrc_negative = mean(frac_nrc_negative),
                                     frac_nrc_positive = mean(frac_nrc_positive)) %>%
    filter(tot_lines > 1000) %>%
    ggplot(aes(frac_nrc_negative, frac_nrc_positive)) + geom_text(aes(label=char))
#Phoebe most extreme pos and neg

#Repeat for Frasier - vader order is Fr, Da, Ni, Ma, Ro
#AFINN order is Da, Fr, Ma, Ro, Ni
#NRC pos-neg order is Fr, Ni, Da, Ro, Ma

#Repeat for WG - vader order is Ja, Ka, Wi, Gr
#AFINN order is Ka, Ja, Wi, Gr
#NRC pos-neg order is Ka, Wi, Ja, Gr

#Repeat for office - vader order is Mi, Ji, An, Pa, Dw
#AFINN order is Pa, Ji, Mi, An, Dw
#NRC pos-neg order is Pa, Mi, Ji, An, Dw

#Repeat for seinfeld - vader order is Kr, Ge, El, Je
#AFINN order is Kr, Ge, El, Je
#NRC pos-neg order is Ge, El, Kr, Je

#Repeat for mwc - vader order is Pe, Ke, Ma, St, Bu, Al
#AFINN order is Pe, Ke, Bu, St, Ma, Al
#NRC pos-neg Pe, Ma, St, Al, Bu, Ke

#Collect all shows - takes 5 minutes
all_sent_res <- data.frame(stringsAsFactors = FALSE)
for (show in c('friends','frasier','wg','office','seinfeld','marriedwc','scrubs')) {
    show_filelist <- Sys.glob(sprintf('./%s_transcripts/refmt_*.txt', show))
    show_res <- data.frame()
    for (f in show_filelist) {
        season <- substr(f, nchar(f)-7, nchar(f)-6)
        ep <- substr(f, nchar(f)-5, nchar(f)-4)
        show_res <- rbind(show_res, data.frame(season = as.integer(season),
                                               ep = as.integer(ep),
                                               get_ep_sent_by_char(f)))
    }
    all_sent_res <- rbind(all_sent_res, data.frame(show=show, show_res, stringsAsFactors = FALSE))
}
all_sent_res %>% group_by(show, char) %>% summarise(n_eps = n(), tot_lines = sum(n_lines), 
                                             frac_nrc_negative = mean(frac_nrc_negative),
                                             frac_nrc_positive = mean(frac_nrc_positive)) %>%
    filter(tot_lines > 2000) %>% 
    ggplot() + ggrepel::geom_text_repel(aes(frac_nrc_negative, frac_nrc_positive, 
                                            label=char, colour=show), force=0, size=4) +
    #better without using repel (force=0)
    geom_abline(linetype=2, slope=1, intercept=0.013) + 
    #xlim(0.020,0.036) + ylim(0.03,0.058) +
    scale_colour_manual(values = colour_dict) + 
    guides(colour='none') + labs(x='Fraction words negative', y='Fraction words positive',
                                 title='Character positive and negative sentiment from NRC') +
    theme_linedraw()
#Characters lie roughly on a line parallel to 1:1 but with higher pos than neg values
#Jerry is on the 1:1 line; Karen, Peggy are shifted higher pos
#Really it is showing show clusters, but with Dwight more neg than other Office and
#  Karen more pos than other WG. Frasier and WG higher on both scores than Friends, Office, Seinfeld:
#  richer writing? Need to see which words cause high scores here

#See the equivalent ridges plot:
all_sent_res %>% 
    mutate(show = ifelse(show=='wg', 'willandgrace', show)) %>%
    group_by(show, season, ep) %>% summarise(sent = weighted.mean(frac_nrc_positive-frac_nrc_negative, n_words)) %>%
    group_by(show) %>% mutate(show_mean_sent = mean(sent)) %>% ungroup() %>%
    mutate(show = fct_reorder(show, show_mean_sent)) %>%
    ggplot() + geom_density_ridges(aes(sent, show, fill=show), alpha=1.0) +#, jittered_points=TRUE, position='raincloud') +
    scale_fill_manual(values = colour_dict) +
    scale_y_discrete(expand = expand_scale(c(0,0))) +#mult = c(0.01, .7))) +
    coord_cartesian(clip='off') +
    theme_minimal() +
    guides(fill='none') + labs(x='Episode fraction positive minus fraction negative words', y='', 
                               title='Show language sentiment from NRC')
#close enough to the Vader order
ggsave('post2_show_ep_sentiments_nrc_ridges.png', width=6, height=4)

#By show season, connecting paths
#Not that useful, but see that Friends got more +ve s8-10 (esp 10),
#  Office s1, Seinfeld s0,1 were outliers; Fraiser, WG, Office most consistent;
#  MWC more extreme (both + and -) in later seasons
#Averge of all shows has no trend over time in pos or neg fractions
all_sent_res %>% group_by(show, season) %>% summarise(n_eps = n(), tot_lines = sum(n_lines), 
                                                    frac_nrc_negative = weighted.mean(frac_nrc_negative,n_lines),
                                                    frac_nrc_positive = weighted.mean(frac_nrc_positive,n_lines)) %>%
    #filter(tot_lines > 1000) %>% 
    ggplot(aes(frac_nrc_negative, frac_nrc_positive)) + 
    geom_path(aes(group=show, colour=season)) + 
    geom_text(aes(label=season, colour=season)) +
    facet_wrap(~show)#+ 
    #geom_abline(linetype=2) + xlim(0.015,0.04) + ylim(0.015,0.075)

#By episode in season - not much interest; pos-neg shows increase to ~20 then decrease, but 
#  afinn just shows slight steady increase
all_sent_res %>% group_by(ep, show) %>% summarise(n_eps = n(), tot_lines = sum(n_lines), 
                                                  frac_nrc_negative = weighted.mean(frac_nrc_negative, n_lines),
                                                  frac_nrc_positive = weighted.mean(frac_nrc_positive, n_lines)) %>%
    ggplot(aes(ep, frac_nrc_positive-frac_nrc_negative)) +
    geom_point(aes(colour=show)) + geom_smooth(aes(group=1), colour='black',
                                               method='loess', se=TRUE)
#Overall by show: WG, MWC highest disgust, Seinfeld way below on joy; Friends least sad
all_sent_res %>% group_by(show) %>% summarise(frac_nrc_disgust = weighted.mean(frac_nrc_disgust, n_lines),
                                              frac_nrc_sadness = weighted.mean(frac_nrc_sadness, n_lines),
                                              frac_nrc_joy = weighted.mean(frac_nrc_joy, n_lines))
tmp <- all_sent_res %>% group_by(char) %>% summarise(frac_nrc_disgust = weighted.mean(frac_nrc_disgust, n_lines),
                                              frac_nrc_sadness = weighted.mean(frac_nrc_sadness, n_lines),
                                              frac_nrc_joy = weighted.mean(frac_nrc_joy, n_lines),
                                              tot_lines = sum(n_lines)) %>%
    filter(tot_lines > 1000)
tmp %>% arrange(-frac_nrc_disgust) %>% head()   #cox, kelso, marcy, turk, karen, angela highest disgust
tmp %>% arrange(-frac_nrc_sadness) %>% head()   #kelso, cox, turk, marcy, jd, carla highest sadness
tmp %>% arrange(-frac_nrc_joy) %>% head()   #turk, marcy, phyllis, grace, jack, phoebe

#
#2) Top words per show by tf-idf -----
all_res_for_tfidf <- data.frame()
for (show in c('friends','frasier','wg','office','seinfeld','marriedwc','scrubs')) {
    show_filelist <- Sys.glob(sprintf('./%s_transcripts/refmt_*.txt', show))
    show_res <- data.frame()
    counter <- 1
    for (filename in show_filelist) {
        tmp <- readLines(filename)
        ep_df <- process_transcripts(tmp)
        
        char_list <- c(unique(ep_df$char), 'peg', 'marty', 'rach', 'kel', 'cox')
        show_res <- rbind(show_res,
                          ep_df %>% unnest_tokens(word, text, token='words') %>% 
                          #ep_df %>% unnest_tokens(word, text, token='ngrams', n=2) %>% 
                              mutate(word = ifelse(word=='ok', 'okay', word)) %>%
                              anti_join(stop_words %>% filter(lexicon=='snowball'),
                                        by='word') %>% 
                              filter(!(word %in% c(char_list, 
                                                   paste0(char_list, '\'s'),
                                                   as.character(seq(0,9)),
                                                   'font',
                                                   'mm','ohh','aah','hmm'))) %>% 
                              count(word)) #%>%
                              #mutate(ep_id = counter))
        counter <- counter + 1
    }
    show_res <- show_res %>% group_by(word) %>% summarise(n=sum(n)) %>% 
        ungroup() %>% mutate(show=show)
    #show_res <- mutate(show_res, show=show)
    all_res_for_tfidf <- rbind(all_res_for_tfidf, show_res)
    rm(show_res, tmp, char_list, ep_df, l)
}
#Keep only words that appear in at least three shows (e.g. no 'mifflin','pheebs')
#Can't order by tf-idf for plot if a word appears in two lists
tmp <- all_res_for_tfidf %>% 
    mutate(show = ifelse(show=='wg', 'willandgrace', show)) %>% 
    group_by(word) %>% filter(n_distinct(show) >= 3) %>% ungroup() %>% 
    bind_tf_idf(word, show, n) %>% 
    #remove a few bad terms that have slipped through; lots of annoying stage directions in Seinfeld can't be cleanly removed 
    filter(!(word %in% c('00','enters','wanders','returns','toward','br'))) %>%
    group_by(show) %>% top_n(10, tf_idf)
tmp %>%
    ggplot() + geom_col(aes(word, tf_idf, fill=show), position='dodge') +
    facet_wrap(~show, scales='free', ncol=4) + 
    scale_fill_manual(values = colour_dict) +
    scale_y_continuous(breaks=c(0,0.0005), labels=c('0','0.0005')) +
    coord_flip() +
    theme_linedraw() +
    theme(legend.position = 'none', plot.title = element_text(size=16)) +
    labs(x='', y='TF-IDF score',
         title = 'Top 10 distinctive words by show')
ggsave('post2_tfidf_bars_by_show.png', width=8, height=4)
#Alt text = Plot showing most distinctive words by tf-idf for seven sitcoms
#Mostly shows names of minor characters - not that interesting
#Collect+calculate by episode rather than entire show?


#3) Stats by show ----
summaries_from_py <- read.csv('sitcom_episode_summary_details.csv', stringsAsFactors = FALSE)
summaries_from_py$text_bow_concat <- NULL
uniq_words_per_ep_show_season <- summaries_from_py %>% arrange(show,season) %>% 
    group_by(show,season) %>%
    summarise(num_chars = median(num_characters)) %>%
    summarise(num_chars_sparkline = as.character(htmltools::as.tags(sparkline(num_chars, width='80px', spotColor=FALSE,
                                                                               chartRangeMin=6, chartRangeMax=17))))

my_out <- summaries_from_py %>% group_by(show) %>% 
    summarise(n_eps = n(), 
              median_lines_per_ep = round(median(tot_lines), 0),
              median_words_per_ep = median(tot_lines * mean_words_per_line),
              mean_word_length = weighted.mean(mean_word_length, tot_lines*mean_words_per_line),
              median_uniq_words_per_ep = median(num_unique_words),
              median_num_characters = median(num_characters),
              mean_noun_verb_ratio = round(mean(noun_verb_ratio),2),
              mean_noun_adjective_ratio = mean(noun_adjective_ratio)) %>%
    arrange(-median_lines_per_ep) %>% data.frame() %>%
    select(show, n_eps, lines_per_ep=median_lines_per_ep, uniq_words_per_ep=median_uniq_words_per_ep,
           noun_verb_ratio = mean_noun_verb_ratio) %>%
    inner_join(uniq_words_per_ep_show_season, by='show') %>% 
    rename(`n_chars_by_season` = num_chars_sparkline) %>%
    mutate(show = ifelse(show=='wg','willandgrace',show)) %>% 
    formattable(align=c('llll'),
                list(
                    show = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
                    uniq_words_per_ep = color_bar('lightgreen'),
                    lines_per_ep = color_bar('violet'),
                    noun_verb_ratio = color_bar('turquoise')))
my_out <- as.htmlwidget(my_out)
my_out$dependencies = c(my_out$dependencies, htmlwidgets:::widget_dependencies("sparkline", "sparkline"))
my_out
#MWC fewer lines pre ep; frasier most words 3100 vs MWC 2200; 3 shows word length ~3.9 vs 4 shows ~4.0;
#  vocab biggest for scrubs, frasier, most characters office & scrubs; higher nv ratio seinfeld, lowest friends;
#  higher na ratio seinfeld, lowest frasier (means uses most adjectives)

#4) Character word length and num unique words ----
#Takes 5 minutes
all_res_for_word_lengths <- data.frame()
for (show in c('friends','frasier','wg','office','seinfeld','marriedwc','scrubs')) {
    this_show_main_chars <- main_chars_list[[show]]
    
    show_filelist <- Sys.glob(sprintf('./%s_transcripts/refmt_*.txt', show))
    show_res <- data.frame()
    for (filename in show_filelist) {
        season <- substr(filename, nchar(filename)-7, nchar(filename)-6)
        tmp <- readLines(filename)
        ep_df <- process_transcripts(tmp)
        #char_list <- unique(ep_df$char)
        show_res <- rbind(show_res,
                          ep_df %>% unnest_tokens(word, text, token='words') %>% 
                              mutate(word = ifelse(word=='ok', 'okay', word)) %>%
                              count(word, char) %>% 
                              mutate(season = season, show = show))
    }
    #add up the by-episode word counts, 
    #  but limit to the first (by episode, i.e. by row) 5000 words per char-season, 
    #  to avoid penalising frequent speakers in the uniq words / total words
    show_res <- show_res %>% group_by(char,season,show) %>% filter(cumsum(n) <= 5000) %>% ungroup() %>%
        group_by(word, char, season, show) %>% 
        summarise(n = sum(n)) %>% ungroup()
    #summarise by character, show, season
    show_res <- show_res %>% group_by(char, season, show) %>% 
        summarise(uniq_words = n(), 
                  tot_words = sum(n), 
                  mean_word_length = weighted.mean(nchar(word), n)) %>% 
        ungroup() %>% 
        mutate(frac_new_words = uniq_words / tot_words) %>% 
        filter(char %in% this_show_main_chars) 
    all_res_for_word_lengths <- rbind(all_res_for_word_lengths, show_res)
    rm(show_res, tmp, ep_df, l)
}
#sort(table(all_res_for_word_lengths$char))

#Plot scatter over for mean of all seasons
all_res_for_word_lengths %>% 
    filter(tot_words >= 3000) %>% 
    group_by(show, char) %>% 
    summarise(mean_word_length = weighted.mean(mean_word_length, tot_words),
              frac_new_words = weighted.mean(frac_new_words, tot_words),
              mean_tot_words_per_season = mean(tot_words)) %>%
    #filter(mean_tot_words_per_season >= 3000) %>% 
    ggplot() + geom_text(aes(frac_new_words, mean_word_length, label=char, colour=show)) +
    geom_smooth(aes(frac_new_words, mean_word_length), method='lm', se=FALSE, linetype=2, colour='black', size=0.6) + 
    scale_colour_manual(values = colour_dict) +
    theme_linedraw() + theme(panel.grid.minor = element_blank()) +
    guides(colour='none') +
    labs(x='Unique words / total words', y='Mean word length / letters', colour='',
         title='Character word length and vocab size') +
    expand_limits(x=c(0.187,0.30))
#Dwight, Andy, Niles furthest top right, Friends bottom left; Niles, Dwight, Frasier longest words; 
#  Jerry outlier long words but small vocab; Peggy, Karen on the opposite side
ggsave('post2_char_word_length_vocab_size_scatter_seasonswge3000.png', width=6, height=4)
#Alt text = Plot showing character word length and vocab size for seven sitcoms

#Need to recalculate for the by-show plot to include all words
all_res_for_word_lengths_by_show <- data.frame()
for (show in c('friends','frasier','wg','office','seinfeld','marriedwc','scrubs')) {

    show_filelist <- Sys.glob(sprintf('./%s_transcripts/refmt_*.txt', show))
    show_res <- data.frame()
    for (filename in show_filelist) {
        season <- substr(filename, nchar(filename)-7, nchar(filename)-6)
        tmp <- readLines(filename)
        ep_df <- process_transcripts(tmp)
        #char_list <- unique(ep_df$char)
        show_res <- rbind(show_res,
                          ep_df %>% unnest_tokens(word, text, token='words') %>% 
                              mutate(word = ifelse(word=='ok', 'okay', word)) %>%
                              count(word) %>% 
                              mutate(season = season, show = show))
    }
    #add up the by-episode word counts
    show_res <- show_res %>%
        group_by(word, season, show) %>% 
        summarise(n = sum(n)) %>% ungroup()
    #summarise by show, season
    show_res <- show_res %>% group_by(season, show) %>% 
        summarise(tot_words = sum(n), 
                  mean_word_length = weighted.mean(nchar(word), n)) %>% 
        ungroup() 
    all_res_for_word_lengths_by_show <- rbind(all_res_for_word_lengths_by_show, 
                                              show_res)
    rm(show_res, tmp, ep_df, l)
}

#Plot word length by show and season
all_res_for_word_lengths_by_show %>% 
    mutate(show = ifelse(show=='wg','willandgrace', show)) %>%
    ggplot(aes(season, mean_word_length, colour=show)) + geom_point(size=3) + geom_path(aes(group=show),size=1) +
    scale_colour_manual(values = colour_dict) +
    theme_linedraw() + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
                             legend.position = c(0.85,0.36)) +
    guides(colour=guide_legend(ncol=2)) +
    labs(x='Season', y='Mean word length / letters', colour='',
         title='Show word lengths over time')
ggsave('post2_show_word_length_lines.png', width=6, height=4)
#Alt text = 

#5) Make table to display rating model results from ipynb
rating_model_results <- rbind(data.frame(model='A', method='Baseline, overall mean', rmse=0.523),
                                         data.frame(model='B', method='Baseline, show means', rmse=0.447),
                                         data.frame(model='C', method='Show, season, text summaries', rmse=0.429),
                                         data.frame(model='D', method='C + character fractions', rmse=0.411),
                                         data.frame(model='E', method='D + term frequencies (reg. GLM)', rmse=0.431),
                                         data.frame(model='F', method='LSTM on transcripts (by scene)', rmse=0.46))
rating_model_results %>% formattable(align=c('lll'),
                                     #table.attr = "class='table table-striped'",
            list(
                model = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
                #rmse = normalize_bar('lightgreen',0.411*2,0.523*2)))
                #rmse = color_tile('lightgreen','grey')))
                rmse = formatter("span", style = x ~ style(display = "block", 
                                                           padding = "0 4px", 
                                                                   `border-radius` = "4px", 
                                                                   `background-color` = csscolor(gradient(pmin(x,0.48),'lightgreen','grey'))))))
#hack colour scheme a bit to allow more difference between the lower 4 rows

#Also fit model D to get p values for terms
ep_summs <- read.csv('/home/david/projects/sitcoms/sitcom_episode_summary_details.csv')
ep_summs$X <- NULL
ep_summs$text_bow_concat <- NULL
ratings = read.csv('/home/david/projects/sitcoms/sitcom_episode_imdb_ratings.csv')
ratings$X <- NULL
ep_summs <- inner_join(ep_summs, ratings, by=c('episode','season','show'))

summary(glm(rating ~ show + season + mean_words_per_line + mean_word_length + num_unique_words + num_characters + noun_verb_ratio + noun_adjective_ratio,
            data = ep_summs, family='gaussian'))
#mean_words_per_line -ve,  mean_word_length -ve, num_unique_words weakly +ve, noun_verb_ratio +ve
summary(glm(rating ~ . - episode - votes, data= ep_summs, family='gaussian'))
#signif +ve for michael, daphne, niles, monica, ross, george, kramer; biggest of those is 1.82 daphne 1.7 kramer
#signif -ve for pam, erin (-2.8), jerry, cox weakly, marcy weakly
#These coefs agree pretty well with the python version on training set only
#  Values for niles_frac are 0 or 0.1-0.3, coef is O(1), i.e. episode is 0.2 higher when he has 0.3 words vs 0.1 words

#Check if sentiment would be predictive
#Sent (Vader) vs rating: slightly negatively correlated if anything. Could be picking up on
#  show mean rating and sentiment differences, e.g. seinfeld low sent high rating.
#In GLM D, fits as -0.84, p=0.02; residual deviance down from 192 to 191.    

#Adding NRC sentiments to glm (D)
summary(glm(rating ~ . - episode - votes, 
            data = inner_join(ep_summs, 
                              all_sent_res %>% rename(episode=ep) %>%
                                  group_by(show,season,episode) %>%
                                  summarise(frac_nrc_sadness = weighted.mean(frac_nrc_sadness, n_lines),
                                            frac_nrc_joy = weighted.mean(frac_nrc_joy, n_lines),
                                            frac_nrc_disgust = weighted.mean(frac_nrc_disgust, n_lines)) %>%
                                  ungroup(), by=c('show','season','episode')),
            family='gaussian'))
#joy, sadness, disgust terms not significant; sadness closest to signif (+ve)

#https://medium.com/@davidmulholland17/sitcoms-natural-language-comparison-e36f8dae2c7e