def plural(count, singular):
    """Turn a singular word into a plural if need be.

    :param count: The number of things (1 yields a singular, everything
        else a plural)
    :param singular: The singular form of the word.
    """

    if count == 1:
        return singular
    else:
        # Woo, irregulars!
        irregulars = {'has': 'have'}
        if singular in irregulars.keys():
            return irregulars[singular]
        else:
            return singular + "s"
