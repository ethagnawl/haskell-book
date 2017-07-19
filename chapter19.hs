-- Exercise

-- In the URL shortener, an important step was omitted. We’re not
-- checking if we’re overwriting an existing short code, which is entirely
-- possible despite them being randomly generated. We can actually
-- calculate the odds of this by examining the cardinality of the values.

-- Hash the URL
-- Used UUIDs
-- add a unix timestamp to the existing key generation approach
