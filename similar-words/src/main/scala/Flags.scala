
object Flags {
  // TODO: Penalty for popular words (the, of, in...)
  // TODO: Non-linear penalty for distance from ngram center.

  val NGRAM_HALF_LENGTH = 2
  val MIN_NGRAM_DIST = 2 * NGRAM_HALF_LENGTH
  val MAX_SIMILAR_WORDS = 20
  val NGRAM_SIMILARITY_NOT_MATCH_PENTALTY_BASE = 2
  val NGRAM_DIST_FROM_CENTER_PENALTY_BASE = 10
  val WORD_SIMILARITY_PROMOTION_BASE = 1.2
}
