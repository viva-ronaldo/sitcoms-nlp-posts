#Notes
#Current version by episode:
# - Friends 3 eps fail: 0623, 0904 (on step3), 0911 (on step2) - all correct
#   - after adding 3 male chars to list, 6 fail (97%), and adding men etc, still 97%
# - Office passes 75%, via pam-angela, pam-erin, pam-phyllis
#     Increasing over time with dip in s4+5 - Pam spending more time with Jim
# - Seinfeld passes 25-50% surprisingly; elaine+anyone else, usually one case per episode
#     e.g. 0410, Elaine and Marla have 3 lines hello/goodbye, then 5 about sex which should flag as about man
#     e.g. 0806 Cynthia and Abby have 3 lines including 'my boyfriend' which should flag; one addresses Jerry, probably shouldn't flag
#     e.g. 0518 Rachel and Helen 4 lines hello/goodbye
#     e.g. 0607 Elaine 6 lines with Hilde and 4 with Kelly the waitress both ordering food
#     e.g. 0716 Helen and Elaine long exchange
# - Will and Grace passes 90%, usually Grace/Karen or Karen/Rosario
#     e.g. 0514 def passes, 0622 two 3-lines, 0112 def passes, 0416 one 4-line
# - MWC passes 84%, with 3 combs of the 3 women
#     e.g. 0602 both convs mention male characters
#     e.g. 0401 one 3-line OK
#   - after adding 3 male chars to exclusion list, qual drops to 72%
#   - after adding men etc., 71%
# - Scrubs passes 72% (half eliott/carla, and some laverne, jordan)
#   - reduced to 70% with male names added, and 70% with men etc 
#     e.g. 0409 has 3-line qualifying but the scene is about a man
#        Still passes in scene version because not discarding whole scenes for mentioning man.
#        Ideally would use POS to recognise 'sexist dirtbag'
# - Frasier passes 45%, increasing over time
#   - reduced to 40% with male names added, and 39% with men etc

#seinfeld 0422 (alison through phone): should give a pass, but is missed as a speaker- could only fix at refmt stage.

#Set up and functions ----
#.libPaths(c(.libPaths(), '/home/david/R/x86_64-pc-linux-gnu-library/3.4/'))
library(dplyr)
library(ggplot2)
library(stringr)
library(forcats)
library(tidytext)
library(tm)
library(topicmodels)

shows_list <- c('friends','frasier','wg','office','seinfeld','marriedwc','scrubs')

main_chars_list <- list('friends' = c("chandler", "joey", "monica", "phoebe", "rachel", "ross"),
                        'frasier' = c("daphne", "frasier", "martin", "niles", "roz"),
                        'wg' = c("grace", "jack", "karen", "will"),
                        'office' = c("dwight", "jim", "michael", "pam", "andy", "darryl", "erin", "ryan"),
                        'seinfeld' = c("elaine", "george", "jerry", "kramer"),
                        'marriedwc' = c("al", "kelly", "marcy", "peggy", "steve", "bud"),
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

#use start years-1 to give a number to add to season to get real year
season_to_year_deltas <- data.frame(show = shows_list, 
                                    years_to_add = c(1993,1994,1997,2004,1986,1990,2001),
                                    stringsAsFactors = FALSE)

source('f_char_lists.R')
f_chars <- unique(c(friends_f_chars, frasier_f_chars, seinfeld_f_chars, wg_f_chars, marriedwc_f_chars, scrubs_f_chars, office_f_chars))
source('m_char_lists.R')
m_chars <- unique(c(friends_m_chars, frasier_m_chars, seinfeld_m_chars, wg_m_chars, marriedwc_m_chars, scrubs_m_chars, office_m_chars))

terms_for_male_pronoun_regex <- c('he','him','his','he\'s',
                                  'ross','chandler','joey',
                                  'j.d.','jd','turk','kelso','cox','dr. kelso','dr. cox','perry','bob','janitor',
                                  'frasier','niles','martin','bulldog','dad',
                                  'bud','jefferson','al',
                                  'michael','jim','dwight','andy','kevin','roy','stanley','ryan',
                                  'will','jack','leo','stan',
                                  'jerry','george','kramer',
                                  'men','a man','a guy','boyfriend','husband','brother','uncle','father','dad','mr.')
male_pronoun_regex <- paste(c('\\b', paste(terms_for_male_pronoun_regex, collapse='\\b|\\b'), '\\b'), collapse='')

#NB this doesn't count 'J.D.'s narration' as a character, which is probably best for this use case
process_transcripts <- function(transcript_lines,
                                to_lower_case = TRUE) {
    ep_df <- data.frame(stringsAsFactors = FALSE)
    scene_num <- 0
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
            if (to_lower_case) l <- tolower(l)
            #
            if (nrow(ep_df) > 0 && char == last(ep_df$char)) {
                #append on to previous line
                ep_df$text[nrow(ep_df)] <- paste(ep_df$text[nrow(ep_df)], l)
            } else {
                ep_df <- rbind(ep_df, data.frame(char=char, text=l, scene=scene_num,
                                                 stringsAsFactors=FALSE))
            }
        } else if (grepl('Scene', l)) {
            scene_num <- scene_num + 1
        }
    }
    ep_df
}

score_episode_bechdel_steps <- function(ep_df, f_chars, male_char_regex,
                                        by_scene = FALSE) {
    step1_n_f_chars <- ep_df %>% filter(char %in% f_chars) %>% 
        pull(char) %>% n_distinct()
    
    if (step1_n_f_chars > 0) {
        ep_df_f_convs <- ep_df %>% mutate(next_speaker = lead(char),
                                          next_line = lead(text),
                                          prev_speaker = lag(char),
                                          prev_line = lag(text),
                                          about_male = grepl(male_char_regex, text, perl=TRUE),
                                          next_line_about_male = grepl(male_char_regex, next_line, perl=TRUE),
                                          prev_line_about_male = grepl(male_char_regex, prev_line, perl=TRUE)) %>% 
            filter(char %in% f_chars & 
                       next_speaker %in% f_chars & 
                       next_speaker != char &
                       prev_speaker == next_speaker)
        
        step2_n_f_conv_pairs <- ep_df_f_convs %>% 
            mutate(speaker_pair = ifelse(prev_speaker > char,
                                         paste(prev_speaker, char, sep='-'),
                                         paste(char, prev_speaker, sep='-'))) %>% 
            pull(speaker_pair) %>% n_distinct()
    } else {
        step2_n_f_conv_pairs <- 0
    }
    
    if (step2_n_f_conv_pairs > 0) {
        step3_n_f_conv_pairs_not_about_male <- ep_df_f_convs %>% 
            filter(!about_male & !next_line_about_male & !prev_line_about_male) %>% 
            mutate(speaker_pair = ifelse(prev_speaker > char,
                                         paste(prev_speaker, char, sep='_'),
                                         paste(char, prev_speaker, sep='_'))) 
        step3_qualifying_speaker_pairs <- unique(step3_n_f_conv_pairs_not_about_male$speaker_pair)
        step3_n_f_conv_pairs_not_about_male <- length(step3_qualifying_speaker_pairs)
        step3_qualifying_speaker_pairs <- paste(step3_qualifying_speaker_pairs, collapse=', ')
    } else {
        step3_n_f_conv_pairs_not_about_male <- 0
        step3_qualifying_speaker_pairs <- ''
    }
    data.frame(step1_n_f_chars = step1_n_f_chars,
               step2_n_f_conv_pairs = step2_n_f_conv_pairs,
               step3_n_f_conv_pairs_not_about_male = step3_n_f_conv_pairs_not_about_male,
               step3_qualifying_speaker_pairs = step3_qualifying_speaker_pairs,
               stringsAsFactors = FALSE)
}

#Still return episode score, but calculate by scene. Scene markings don't always work 
#  so this will sometimes fall back to being similar to by episode.
score_scene_bechdel_steps <- function(ep_df, f_chars, male_char_regex,
                                        by_scene = FALSE) {
    ep_df$line_num <- seq_along(ep_df$char)
    ep_df_step1 <- ep_df %>% group_by(scene) %>% filter(sum(unique(char) %in% f_chars) >= 2) %>% ungroup()
    step1_n_scenes <- n_distinct(ep_df_step1$scene)
    
    if (step1_n_scenes >= 1) {
        #have to make one df for steps 2 and 3 because can't drop lines from 2 before doing 3
        ep_df_step2and3 <- ep_df_step1 %>% arrange(line_num) %>% group_by(scene) %>% 
            mutate(next_speaker = lead(char),
                   next_line = lead(text),
                   prev_speaker = lag(char),
                   prev_line = lag(text),
                   about_male = grepl(male_char_regex, text, perl=TRUE),
                   next_line_about_male = grepl(male_char_regex, next_line, perl=TRUE),
                   prev_line_about_male = grepl(male_char_regex, prev_line, perl=TRUE)) 
        
        step2_n_scenes <- ep_df_step2and3 %>% 
            filter(char %in% f_chars & 
                       (row_number()==n() | next_speaker %in% f_chars) & 
                       (row_number()==n() | next_speaker != char) &
                       (row_number()==1 | prev_speaker == next_speaker)) %>%
            pull(scene) %>% n_distinct()
        #leaves at least the middle line of a female 3-line; 
        #  will miss the edge lines where it moves to another character just after, unless at start/end scene
    } else {
        step2_n_scenes <- 0
    }
    
    if (step2_n_scenes > 0) {
        #we need to find at least 1 line per scene that is f and not about m,
        #  and surrounded by lines that are f and not about m, giving a 3-line group;
        #  therefore the first/last lines in scene can't fulfil this, so don't use row_number
        ep_df_step3 <- ep_df_step2and3 %>% 
            filter(char %in% f_chars & !about_male &
                       next_speaker %in% f_chars & !next_line_about_male & next_speaker != char &
                       prev_speaker == next_speaker & !prev_line_about_male) %>% 
            mutate(speaker_pair = ifelse(prev_speaker > char,
                                         paste(prev_speaker, char, sep='_'),
                                         paste(char, prev_speaker, sep='_'))) %>% ungroup()
        
        step3_n_scenes <- n_distinct(ep_df_step3$scene)
        step3_qualifying_speaker_pairs <- unique(ep_df_step3$speaker_pair)
        #step3_n_f_conv_pairs_not_about_male <- length(step3_qualifying_speaker_pairs)
        step3_qualifying_speaker_pairs <- paste(step3_qualifying_speaker_pairs[!is.na(step3_qualifying_speaker_pairs)], collapse=', ')
        #Text from the qualifying exchanges is 'text' in the remaining rows, plus the 
        #  first prev_line and last next_line in each exchange - but need to group by 
        #  scene and speaker; even then it won't be right sometimes if there are
        #  two exchanges by same pair in a scene, but this will do.
        step3_qualifying_text_convs <- ep_df_step3 %>% group_by(scene, speaker_pair) %>% 
            summarise(text = paste(first(prev_line), paste(text, collapse=' '), last(next_line)),
                      .groups = 'drop_last')
    } else {
        step3_n_scenes <- 0
        step3_qualifying_speaker_pairs <- ''
        step3_qualifying_text_convs <- data.frame(scene=c(), text=c())
    }
    list('stats' = data.frame(ep_tot_scenes = n_distinct(ep_df$scene),
               step1_n_scenes = step1_n_scenes,
               step2_n_scenes = step2_n_scenes,
               step3_n_scenes = step3_n_scenes,
               step3_qualifying_speaker_pairs = step3_qualifying_speaker_pairs,
               stringsAsFactors = FALSE),
         'text' = step3_qualifying_text_convs)
}

#Now a rough calculation of number lines in back and forths between each gender pair
score_ep_conv_gender_pair_lines <- function(ep_df, f_chars, m_chars) {
    ep_df$line_num <- seq_along(ep_df$char)
    
    ep_df %>% arrange(line_num) %>% group_by(scene) %>% 
        mutate(next_speaker = lead(char),
               prev_speaker = lag(char),
               this_speaker_g = ifelse(char %in% f_chars, 'f',
                                       ifelse(char %in% m_chars, 'm', 'u')),
               next_speaker_g = ifelse(next_speaker %in% f_chars, 'f',
                                       ifelse(next_speaker %in% m_chars, 'm', 'u')),
               prev_speaker_g = ifelse(prev_speaker %in% f_chars, 'f',
                                       ifelse(prev_speaker %in% m_chars, 'm', 'u'))) %>%
        filter((row_number()==n() | next_speaker != char) &
                   (row_number()==1 | prev_speaker == next_speaker)) %>%
        mutate(speaker_pair = ifelse(prev_speaker > char,
                                     paste(prev_speaker, char, sep='_'),
                                     paste(char, prev_speaker, sep='_')),
               g_pair = ifelse(this_speaker_g=='m' & prev_speaker_g=='m', 'm-m',
                               ifelse(this_speaker_g=='f' & prev_speaker_g=='f', 'f-f',
                                      ifelse(this_speaker_g %in% c('m','f') &
                                                 prev_speaker_g %in% c('m','f'), 'm-f', 'u')))) %>%
        select(scene, speaker_pair, g_pair) %>% 
        ungroup() %>% 
        #filter(!duplicated(.)) %>%
        filter(g_pair != 'u') %>%
        summarise(n_ff = sum(g_pair=='f-f'),
                  n_mm = sum(g_pair=='m-m'),
                  n_mf = sum(g_pair=='m-f'))
}


# Testing code ----

# First get all characters and create a list of females manually, and males
#assume the characters have to be named, not waitresses etc.
#show_filelist <- Sys.glob(sprintf('./%s_transcripts/refmt_*.txt', 'office'))
#all_chars <- lapply(show_filelist, function(f) unique(process_transcripts(readLines(f))$char)) %>%
#    unlist() %>% unique() %>% sort()

ep1 <- process_transcripts(readLines('friends_transcripts/refmt_0101.txt'))
unique(ep1$char)

#by episode
#1: has two female chars
ep1 %>% filter(char %in% f_chars) %>% summarise(n_f_chars = n_distinct(char))
#if > 0, passes 1
#2: they talk to each other - require a back and forth, especially important in a group
#   TODO or should it be 3 lines in a row all female?
ep1 %>% mutate(next_speaker = lead(char),
               prev_speaker = lag(char)) %>% 
    filter(char %in% f_chars & 
               next_speaker %in% f_chars & 
               next_speaker != char &
               prev_speaker == next_speaker) %>% 
    mutate(speaker_pair = ifelse(prev_speaker > char,
                                 paste(prev_speaker, char, sep='-'),
                                 paste(char, prev_speaker, sep='-'))) %>% 
    summarise(n_f_convs = n_distinct(speaker_pair))
#if > 0, passes 2
#3: talk about not a male character
ep1 %>% mutate(next_speaker = lead(char),
               next_line = lead(text),
               prev_speaker = lag(char),
               prev_line = lag(text),
               about_male = grepl(male_pronoun_regex, text, perl=TRUE),
               next_line_about_male = grepl(male_pronoun_regex, next_line, perl=TRUE),
               prev_line_about_male = grepl(male_pronoun_regex, prev_line, perl=TRUE)) %>% 
    filter(char %in% f_chars & 
               next_speaker %in% f_chars & 
               next_speaker != char &
               prev_speaker == next_speaker) %>% 
    #sample_n(5) %>% 
    summarise(n_lines_in_conv_not_about_male = sum(!about_male & !next_line_about_male & !prev_line_about_male))
#if > 0, passes

# By scene, step 3
# Currently this doesn't exclude a scene if part of it talks about a man,
#   as long as 3 female lines don't mention him
# ep1 %>% mutate(n_scenes = max(scene)) %>% 
#     group_by(scene) %>% 
#     mutate(next_speaker = lead(char),
#            next_line = lead(text),
#            prev_speaker = lag(char),
#            prev_line = lag(text),
#            about_male = grepl(male_pronoun_regex, text, perl=TRUE),
#            next_line_about_male = grepl(male_pronoun_regex, next_line, perl=TRUE),
#            prev_line_about_male = grepl(male_pronoun_regex, prev_line, perl=TRUE)) %>% 
#     ungroup() %>%
#     filter(char %in% f_chars & 
#            (is.na(next_speaker) | next_speaker %in% f_chars) & 
#            (is.na(next_speaker) | next_speaker != char) &
#            (is.na(prev_speaker) | prev_speaker == next_speaker)) %>% 
#     #this leaves some scenes still in play
#     group_by(scene) %>%
#     summarise(has_line_about_male = any(about_male),
#               n_scenes = first(n_scenes)) %>%
#     summarise(frac_scenes_pass = sum(!has_line_about_male) / first(n_scenes))


show <- 'frasier'
terms_for_male_pronoun_regex <- c('he','him','his','he\'s',
                                  #'ross','chandler','joey',
                                  'j.d.','jd','turk','kelso','cox','dr. kelso','dr. cox','perry','bob','janitor',
                                  #'frasier','niles','martin','bulldog','dad',
                                  #'bud','jefferson','al',
                                  'men','a man','a guy','boyfriend','husband','brother','uncle','father','dad','mr.')
male_pronoun_regex <- paste(c('\\b', paste(terms_for_male_pronoun_regex, collapse='\\b|\\b'), '\\b'), collapse='')

show_filelist <- Sys.glob(sprintf('./%s_transcripts/refmt_*.txt', show))
res <- data.frame()
for (f in show_filelist) {
    show <- show
    season <- substr(f, nchar(f)-7, nchar(f)-6)
    ep <- substr(f, nchar(f)-5, nchar(f)-4)
    res <- rbind(res, data.frame(show = show,
                                 season = as.integer(season), 
                                 ep = as.integer(ep),
                                 score_episode_bechdel_steps(process_transcripts(readLines(f)), f_chars, male_pronoun_regex),
                                 stringsAsFactors = FALSE))
}
#Overall statistics
res %>% group_by(show) %>% 
    summarise(n_eps = n(),
              frac_pass_step1 = mean(step1_n_f_chars >= 2),
              frac_pass_step2 = mean(step2_n_f_conv_pairs >= 1),
              frac_pass_step3 = mean(step3_n_f_conv_pairs_not_about_male >= 1),
              median_conv_pairs_when_passing = median(ifelse(step3_n_f_conv_pairs_not_about_male >= 1, step3_n_f_conv_pairs_not_about_male, NA), na.rm=T))
#Most common pairs passing test
res %>% filter(show=='frasier') %>% 
    select(season, ep, step3_qualifying_speaker_pairs) %>% mutate(tot_eps = n()) %>% 
    unnest_tokens(speaker_pair, step3_qualifying_speaker_pairs, token= function(l) strsplit(l, ', ')) %>%
    group_by(speaker_pair) %>%
    summarise(n_passes = n(), frac_eps_passed = n_passes / first(tot_eps)) %>%
    arrange(-frac_eps_passed)
    
#Plot passing step 3 by show and real year
res %>% inner_join(season_to_year_deltas, by='show') %>%
    mutate(real_year = season + years_to_add) %>%
    group_by(show, real_year) %>% 
    summarise(frac_pass_step3 = mean(step3_n_f_conv_pairs_not_about_male >= 1)) %>%
    ggplot(aes(real_year, frac_pass_step3, colour=show)) + geom_point() + geom_path(aes(group=show)) +
    scale_colour_manual(values = colour_dict) +
    expand_limits(y=c(0,1)) +
    labs(x='', y='Fraction episodes passing', colour='',
         title='Sitcom episodes passing Bechdel test by year') +
    theme_minimal() +
    theme(legend.position='bottom')

# All shows, breaking up by scene first ----
terms_for_male_pronoun_regex <- c('he','him','his','he\'s',
                                  'ross','chandler','joey',
                                  'j.d.','jd','turk','kelso','cox','dr. kelso','dr. cox','perry','bob','janitor',
                                  'frasier','niles','martin','bulldog','dad',
                                  'bud','jefferson','al',
                                  'michael','jim','dwight','andy','kevin','roy','stanley','ryan',
                                  'will','jack','leo','stan',
                                  'jerry','george','kramer',
                                  'men','a man','a guy','boyfriend','husband','brother','uncle','father','dad','mr.')
male_pronoun_regex <- paste(c('\\b', paste(terms_for_male_pronoun_regex, collapse='\\b|\\b'), '\\b'), collapse='')

#takes 10mins
res_scenes <- data.frame()
qual_text_convs <- data.frame()
#for (show in c('seinfeld')) {
for (show in shows_list) {
    show_filelist <- Sys.glob(sprintf('./%s_transcripts/refmt_*.txt', show))
    for (f in show_filelist) {
        show <- show
        season <- substr(f, nchar(f)-7, nchar(f)-6)
        ep <- substr(f, nchar(f)-5, nchar(f)-4)
        
        one_res_scenes <- score_scene_bechdel_steps(process_transcripts(readLines(f)), f_chars, male_pronoun_regex)
        
        res_scenes <- rbind(res_scenes, data.frame(show = show,
                                     season = as.integer(season), 
                                     ep = as.integer(ep),
                                     one_res_scenes$stats,
                                     stringsAsFactors = FALSE))
        if (nrow(one_res_scenes$text) > 0) { 
            qual_text_convs <- rbind(qual_text_convs, 
                                     data.frame(show = show,
                                                season = as.integer(season), 
                                                ep = as.integer(ep),
                                                one_res_scenes$text,
                                                stringsAsFactors = FALSE))
        }
    }
}
#Overall statistics
res_scenes %>% group_by(show) %>% 
    summarise(n_eps = n(),
              frac_pass_step1 = mean(step1_n_scenes >= 1),
              frac_pass_step2 = mean(step2_n_scenes >= 1),
              frac_pass_step3 = mean(step3_n_scenes >= 1),
              median_scenes_when_passing = median(ifelse(step3_n_scenes >= 1, step3_n_scenes, NA), na.rm=T))
#friends almost always passes with multiple scenes; wg second; 
#  frasier, seinfeld pass step 1 often but fall at step 2 - could be processing flaws? e.g scene markers, 3-line rule
#  Frasier seems right. Many multi-person scenes; some in coffee shop with comings and goings
#    Usually women revolve around Frasier, e.g. rarely do his girlfriends talk to R/D
#  Seinfeld scene breaks inconsistent so sometimes step 1 is 2 women in episode


#Most common pairs passing test
res_scenes %>% filter(show=='office', season==8) %>%
    select(season, ep, step3_qualifying_speaker_pairs) %>% mutate(tot_eps = n()) %>%
    unnest_tokens(speaker_pair, step3_qualifying_speaker_pairs, token= function(l) strsplit(l, ', ')) %>%
    group_by(speaker_pair) %>%
    summarise(n_passes = n(), frac_eps_passed = n_passes / first(tot_eps)) %>%
    arrange(-frac_eps_passed)

#Plot passing step 3 by show and real year
res_scenes %>% inner_join(season_to_year_deltas, by='show') %>%
    mutate(show = ifelse(show=='wg', 'willandgrace', show)) %>%
    mutate(real_year = season + years_to_add) %>%
    group_by(show, real_year) %>% 
    summarise(frac_pass_step3 = mean(step3_n_scenes >= 1), .groups='drop_last') %>%
    ggplot(aes(real_year, frac_pass_step3, colour=show)) + 
    geom_point(size=1.5) + 
    geom_path(aes(group=show),size=0.8) +
    scale_colour_manual(values = colour_dict) +
    expand_limits(y=c(0,1)) +
    labs(x='', y='Fraction episodes passing', colour='',
         title='Sitcom episodes passing Bechdel test by year') +
    theme_minimal() +
    #theme(legend.position='bottom') +
    theme(legend.position = c(0.7,0.13)) +
    guides(colour = guide_legend(nrow=2, byrow=TRUE)) +
    ggplot2::annotate(geom = "curve", x = 2009.5, y = 0.3, xend = 2008.2, yend = 0.45, 
        curvature = -.3, arrow = arrow(length = unit(2, "mm"))) +
    ggplot2::annotate("text", x = 2009.8, y = 0.3, label = "Karen leaves;\nJim & Pam together", 
                      hjust = "left", size=3) +
    ggplot2::annotate(geom = "curve", x = 2006.5, y = 0.93, xend = 2004.5, yend = 0.83, 
                      curvature = 0, arrow = arrow(length = unit(2, "mm"))) +
    ggplot2::annotate("text", x = 2006.7, y = 0.95, label = "Gertrude living\nwith Daphne;\nJulia at KACL", hjust = "left", size=3) +
    ggplot2::annotate(geom = "curve", x = 1992.3, y = 0.18, xend = 1994.2, yend = 0.22, 
                      curvature = 0, arrow = arrow(length = unit(2, "mm"))) +
    ggplot2::annotate("text", x = 1993, y = 0.12, label = "Elaine stops talking\nto other women", hjust = "right", size=3)
ggsave('./post_bechdel_eps_passing_by_year_annotated.png', width=7.3, height=4)
#If you only have 2 main F characters, hard to consistently pass,
#  except for WG where they work together directly (unlike Elliot & Carla).
#
#Why spike in Frasier s11, drop in Seinfeld s9? It is in step 2 in both cases.
#  Seinfeld s9 only 3 passes- elaine never talking to women? Seems right.
#  Frasier s10 it is because Gertrude lived with D for a while and Julia worked with R.
#    R-D rate was similar in s9-10 at 0.3. In s11 it drops to 0.08 (2 eps only).
#  And MWC s10 drop - 2/9 matches. no peggy-marcy.
#  Office s3 vs 4-5 is pam-karen in s3, gone s4-5 plus drop-off in pam-angela s4-5

#Some overall plot of fraction passing stage 2, then passing stage 3 with different regexes (just pronouns, incl names, incl men etc)
res_scenes %>% mutate(show=ifelse(show=='wg','willandgrace',show)) %>%
#    filter(ep_tot_scenes > 3) %>% 
    group_by(show) %>% 
    summarise(step1_frac_eps = mean(ifelse(ep_tot_scenes > 3, step1_n_scenes >= 1, NA), na.rm=T),
              step2_frac_eps = mean(step2_n_scenes >= 1),
              step2_frac_scenes = mean(step2_n_scenes / ep_tot_scenes),
              step3_frac_eps = mean(step3_n_scenes >= 1),
              step3_frac_scenes = mean(step3_n_scenes / ep_tot_scenes)) %>%
    mutate(show = fct_reorder(show, -step3_frac_eps)) %>% 
    ggplot() + geom_linerange(aes(show, ymin=step3_frac_eps, ymax=step1_frac_eps,
                                   colour=show), size=0.5) +
    #geom_pointrange(aes(show, y=step1_frac_eps, ymin=step2_frac_eps, ymax=step1_frac_eps,
    #                    colour=show), size=1, shape='o') +
    #geom_point(aes(show, step1_frac_eps), colour='white', size=5) +
    geom_point(aes(show, step1_frac_eps, colour=show), size=5) +
    geom_point(aes(show, step1_frac_eps), shape='1', size=3, colour='white') +
    geom_point(aes(show, step2_frac_eps, colour=show), size=5) +
    geom_point(aes(show, step2_frac_eps), shape='2', size=3, colour='white') +
    geom_point(aes(show, step3_frac_eps, colour=show), size=5) +
    geom_point(aes(show, step3_frac_eps), shape='3', size=3, colour='white') +
    ggplot2::annotate("text", x = 4.6, y = 0.77, label = "Women talking,\nbut about men", hjust = "left", size=2.2) +
    ggplot2::annotate("text", x = 6.05, y = 0.66, label = "Women in scene,\nbut not talking\nto each other", hjust = "left", size=2.2) +
    scale_colour_manual(values = colour_dict) +
    guides(colour='none') +
    ylim(0,1) +
    labs(x='', y='Fraction episodes passing', colour='',
         title='Episodes passing Bechdel steps 1, 2, 3',
         subtitle='Episodes with three or fewer scenes in transcript omitted from step 1, which is all for \'office\'') +
    #coord_flip() +
    theme_minimal() + theme(axis.text.x = element_text(angle=45, hjust=1), plot.subtitle=element_text(size=7))
#ggsave('./post_bechdel_eps_passing_3_steps_annotated_noofficestep1.png', width=6, height=4)
#Married wC and Scrubs are notable for having 2 women talking to each other often
#  but failing to have them talk about not men.
#Office is all 1 scene eps; Scrubs, Seinfeld, and possibly Frasier have some <= 3 scene eps that are missing scene breaks
#  but including/excluding them doesn't change step 1 values much.


#Other plots
#DONOE stacked bars of frac passing male equiv test
#DONE frac passing eps over time for key F pairs: 3 Friends, Roz-Daphne,
#    Pam-Angela?, Kelly-Peggy, Marcy-Peggy, Elliot-Carla, Karen-Grace, Karen-Rosario
#    May be too noisy by season. Table for overall results?
#- 2d snakes of frac eps passing vs m/f line ratio by season (too correlated?)
#- something showing who shared scenes with whom
#- heat map of show-topic if tm works

res_scenes %>%
    group_by(show) %>% mutate(show_tot_eps = n()) %>% ungroup() %>% 
    select(show, show_tot_eps, season, ep, step3_qualifying_speaker_pairs) %>% 
    unnest_tokens(speaker_pair, step3_qualifying_speaker_pairs, token= function(l) strsplit(l, ', ')) %>%
    group_by(show, speaker_pair) %>%
    summarise(n_passes = n(), frac_eps_passed = n_passes / first(show_tot_eps)) %>%
    arrange(-frac_eps_passed)
#3 Friends combs passing 0.6 eps each; wg G+K 0.6 plus K+R 0.2;
#  Scrubs E+C 0.4; MWC 2 pairs 0.3-0.4; Office relies on P+A 0.2 and less

# Topic model on the qualifying conversations, with all names removed ----

#Conversations have average ~50 words
#Most common words after filtering stop words appear 100-200 times total
other_words_to_remove <- c('yeah', 'hey', 'gonna', 'umm', 'wanna', 
                           'put', 'ooh', 'ohh', 'gotta', 'god',
                           'honey', 'good', 'great', 'huh', 'wow',
                           'talk', 'make', 'give', 'guys', 'time',
                           'nbsp')
qual_text_corpus <- VCorpus(VectorSource(qual_text_convs$text)) %>%
    tm_map(stripWhitespace) %>%
    #tm_map(removeWords, stopwords('english')) %>%
    tm_map(removeWords, subset(stop_words, lexicon=='SMART')$word) %>%
    tm_map(removePunctuation, 
           preserve_intra_word_dashes = TRUE) %>%
    tm_map(removeWords, as.character(unlist(main_chars_list))) %>%
    tm_map(removeWords, other_words_to_remove)
qual_text_dtm <- DocumentTermMatrix(qual_text_corpus,
                                    control = list(bounds = list(global=c(10, nrow(qual_text_convs)/5)))) #terms in 5 < n_docs < Inf
#see getTransformations() for tm list
#tm_map(reuters, removeWords, stopwords("english"))
#tm_map(reuters, stemDocument)
#general processing like tm_map(reuters, content_transformer(R_text_function))

#sparse terms now handled in dtm above
#qual_text_dtm <- removeSparseTerms(qual_text_dtm, 0.99)

#Some rows of qual_text_dtm may be zero, if the text contains only names,
#  stopwords, or words filtered out in previous step.
tmp <- as.matrix(qual_text_dtm)
rows_kept_for_LDA <- which(rowSums(tmp) > 0)
qual_text_dtm <- qual_text_dtm[rows_kept_for_LDA, ]
rm(tmp)

#Some words are very common but are getting into topics
#Remove (yeah, hey, gonna, umm, wanna, put, ooh, ohh, gotta, god) above
tm::findMostFreqTerms(qual_text_dtm, n=50, INDEX=rep(1,nDocs(qual_text_dtm)))
#

qual_text_lda <- LDA(qual_text_dtm, k=10, method='Gibbs', 
                     control=list(alpha=0.02,seed=3402))
#Does it separate docs at all?
posterior(qual_text_lda)$topics[1:5,]
mean(posterior(qual_text_lda)$topics > 2/attr(qual_text_lda,'k'))
#Gibbs seems to make it much more confident
#Tune alpha using this?
perplexity(qual_text_lda, newdata=qual_text_dtm)

#top terms by topic
terms(qual_text_lda, k=6)
#assign documents to topics; each document is a scene-speaker_pair
qual_text_convs$most_likely_topic <- ''
qual_text_convs$most_likely_topic[rows_kept_for_LDA] <- as.character(topics(qual_text_lda, k=1,
                                                                            threshold=0.5))
table(qual_text_convs$show, qual_text_convs$most_likely_topic)
qual_text_convs %>% sample_n(1) %>% select(most_likely_topic, text)

# library(LDAvis)
# # Find required quantities
# phi <- posterior(qual_text_lda)$terms %>% as.matrix
# theta <- posterior(qual_text_lda)$topics %>% as.matrix
# vocab <- colnames(phi)
# doc_length <- vector()
# #Reduce size of qual_text_corpus by excluding words not in qual_text_dtm
# for (i in rows_kept_for_LDA) {
#     temp <- paste(qual_text_corpus[[i]]$content, collapse = ' ')
#     doc_length <- c(doc_length, stringi::stri_count(temp, regex = '\\S+'))
# }
# # Convert to json
# json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
#                                vocab = vocab,
#                                doc.length = doc_length,
#                                term.frequency = as.integer(colSums(as.matrix(qual_text_dtm))))
# serVis(json_lda)

#library(topicdoc)  #3.5 only
#keyATM looks good too, 3.5 only
#ldatuning to tune k using 4 different metrics from literature - just runs LDA in a parallelised loop
#  install fails due to GNU MP not found

#-> Nothing useful from LDA

# Apply the reverse of the test ----
#Rough first version using only main male characters
main_male_chars <- setdiff(as.character(unlist(main_chars_list)), f_chars)
terms_for_female_pronoun_regex <- c('she','her','she\'s',
                                    f_chars,
                                    'women','a woman','a girl','girlfriend','wife',
                                    'sister','aunt','mother','mom','ma','mrs.')
female_pronoun_regex <- paste(c('\\b', paste(terms_for_female_pronoun_regex, collapse='\\b|\\b'), '\\b'), collapse='')

#Takes 10 mins
male_res_scenes <- data.frame()
male_qual_text_convs <- data.frame()
for (show in shows_list) {
    show_filelist <- Sys.glob(sprintf('./%s_transcripts/refmt_*.txt', show))
    for (f in show_filelist) {
        show <- show
        season <- substr(f, nchar(f)-7, nchar(f)-6)
        ep <- substr(f, nchar(f)-5, nchar(f)-4)
        
        one_res_scenes <- score_scene_bechdel_steps(process_transcripts(readLines(f)), 
                                                    m_chars, 
                                                    female_pronoun_regex)
        
        male_res_scenes <- rbind(male_res_scenes, data.frame(show = show,
                                                             season = as.integer(season), 
                                                             ep = as.integer(ep),
                                                             one_res_scenes$stats,
                                                             stringsAsFactors = FALSE))
        if (nrow(one_res_scenes$text) > 0) { 
            male_qual_text_convs <- rbind(male_qual_text_convs, 
                                     data.frame(show = show,
                                                season = as.integer(season), 
                                                ep = as.integer(ep),
                                                one_res_scenes$text,
                                                stringsAsFactors = FALSE))
        }
    }
}
#Overall statistics, male version:
male_res_scenes %>% group_by(show) %>% 
    summarise(n_eps = n(),
              frac_pass_step1 = mean(step1_n_scenes >= 1),
              frac_pass_step2 = mean(step2_n_scenes >= 1),
              frac_pass_step3 = mean(step3_n_scenes >= 1),
              median_scenes_when_passing = median(ifelse(step3_n_scenes >= 1, step3_n_scenes, NA), na.rm=T))
#Seinfeld passes 98% episodes

#Stacked bar for passes both, male only, female only, neither, by episode
res_scenes %>% mutate(pass_female = step3_n_scenes > 0) %>% select(show,season,ep,pass_female) %>%
    inner_join(
        male_res_scenes %>% mutate(pass_male = step3_n_scenes > 0) %>% select(show,season,ep,pass_male),
        by = c('show','season','ep')
    ) %>%
    mutate(verdict = ifelse(pass_female & pass_male, 'passes_both',
                            ifelse(pass_female, 'passes_female',
                                   ifelse(pass_male, 'passes_male', 'fails')))) %>%
    mutate(show = ifelse(show == 'wg', 'willandgrace', show),
           verdict = factor(verdict, levels=rev(c('passes_both','passes_female','passes_male','fails'))),
           show = factor(show, levels=rev(c('friends','willandgrace','office','scrubs','marriedwc','frasier','seinfeld')))) %>% 
    ggplot() + stat_count(aes(show, fill=verdict, group=verdict), 
                          position=position_fill(),
                          width=0.5) +
    scale_fill_manual(values = c('dimgrey','skyblue','coral','mediumpurple')) +
    labs(x='', y='Fraction of episodes', fill='',
         title='Episodes passing female and male conversation tests') +
    coord_flip() +
    guides(fill = guide_legend(reverse=TRUE)) +
    theme_minimal() + theme(panel.grid.minor = element_blank()) 
ggsave('./post_bechdel_female_vs_male_test_stacked_bars.png', width=6, height=5)
#MWC has most female only; e.g. s4e5, s4e21, s11e22, all look right; also friends s6e10, s7e19 are right.

#Who is talking to whom
ep_conv_pairs <- data.frame()
for (show in shows_list) {
    show_filelist <- Sys.glob(sprintf('./%s_transcripts/refmt_*.txt', show))
    for (f in show_filelist) {
        show <- show
        season <- substr(f, nchar(f)-7, nchar(f)-6)
        ep <- substr(f, nchar(f)-5, nchar(f)-4)
        
        one_ep_conv_pairs <- score_ep_conv_gender_pair_lines(process_transcripts(readLines(f)), f_chars, m_chars)
        
        ep_conv_pairs <- rbind(ep_conv_pairs, data.frame(show = show,
                                                   season = as.integer(season), 
                                                   ep = as.integer(ep),
                                                   one_ep_conv_pairs,
                                                   stringsAsFactors = FALSE))
    }
}
ep_conv_pairs %>% group_by(show) %>% 
    summarise(mean_mm = mean(n_mm), mean_ff = mean(n_ff), mean_mf = mean(n_mf),
              diffg_to_sameg_ratio = sum(n_mf)/sum(n_ff+n_mm),
              mm_to_ff_ratio = sum(n_mm)/sum(n_ff))
ep_conv_pairs %>% group_by(show) %>% 
    summarise(diffg_to_sameg_ratio = sum(n_mf)/sum(n_ff+n_mm),
              mm_to_ff_ratio = sum(n_mm)/sum(n_ff)) %>% 
    ggplot() + 
    geom_hline(yintercept = 1, linetype = 3) + geom_vline(xintercept = 1, linetype = 3) +
    geom_label(aes(x=mm_to_ff_ratio, y=diffg_to_sameg_ratio, label=show, colour=show)) +
    scale_x_continuous(breaks=c(1,10,20), limits=c(-1,27)) + 
    scale_y_continuous(breaks=seq(0.6,1.8,0.20), limits=c(0.75,1.78)) +
    scale_colour_manual(values = colour_dict) +
    guides(colour='none') +
    labs(x='Male-male to female-female ratio',
         y='Different gender to same gender ratio',
         title='Back-and-forth conversation lines by gender pairing') +
    theme_light()
#ggsave('./post_bechdel_conv_ratio_scatter.png', width=6, height=5)
#marriedwc, wg, friends have more unique instances of m talking to f than mm+ff.
#  Mainly because there are more pairings available for mf when there are equal numbers
#    of m,f characters. 
#frasier, seinfeld, office have <1 ff per episode so mm/ff is >>1. All have plenty of mf though.

#Post plan
#- Technically could use POS to identify subjects in lines, but simpler key word search seems to work well
#- Plot of passing steps 1-3; comment step 1 not reliable for office, seinfeld; comment on how each passes
#- Plot of passing step 3 vs time; comment on specific points
#- Something about topics when passing if it works
#- Male equivalent; stacked bar plot?



