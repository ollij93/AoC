import logging

_logger = logging.getLogger()


def _p1_single_parser(data: str) -> tuple[int, str]:
    """
    Worked function for part 1 which will attempt to parse for the first
    mul(##,##) pattern.
    If the pattern is found, it performs the multiplication and returns the value.
    Otherwise it returns zero.
    The function also always returns the remainder of the string that it hasn't processed.
    """
    """
    Parse for mul(##,##) patterns, multiplying and summing.

    Use a recursive pattern to simplify the parsing greatly.
    """
    if not data.startswith("mul("):
        # Drop the front character and retry the parsing
        _logger.debug(data, "does not start 'mul('")
        return 0, data[1:]

    data = data[4:]
    # Get the number inside the multiplication
    numAstr = ""
    while data[0].isdigit():
        numAstr += data[0]
        data = data[1:]
    # If no number found, or we stopped on a unexpected char, restart the
    # parsing.
    _logger.debug(numAstr, "read as numA")
    _logger.debug("followed by:", data[0])
    if not numAstr or data[0] != ",":
        return 0, data

    # Drop the ","
    data = data[1:]
    # At this point have parsed mul(##,
    numBstr = ""
    while data[0].isdigit():
        numBstr += data[0]
        data = data[1:]

    _logger.debug(numBstr, "read as numB")
    _logger.debug("followed by:", data[0])
    # Again, if no number found, or we stopped on an unexpected char, restart
    # the parsing.
    if not numBstr or data[0] != ")":
        return 0, data

    # Drop the ")"
    data = data[1:]
    _logger.debug("SUCCESSFUL PARSE FOR MUL", numAstr, "*", numBstr)
    # Successful parse... do the multiplication and process the rest of the string
    return (int(numAstr) * int(numBstr)), data


def p1(data: str) -> int:
    data = data.strip()
    total = 0
    while data:
        num, data = _p1_single_parser(data)
        total += num
    return total


def p2(data: str) -> int:
    data = data.strip()
    total = 0
    do = True
    while data:
        if data.startswith("do()"):
            do = True
            data = data[4:]
        if data.startswith("don't()"):
            do = False
            data = data[7:]
        num, data = _p1_single_parser(data)
        if do:
            total += num
    return total
