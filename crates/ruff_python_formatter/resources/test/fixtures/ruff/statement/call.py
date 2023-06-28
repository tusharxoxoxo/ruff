from unittest.mock import MagicMock


def f(*args, **kwargs):
    pass

this_is_a_very_long_argument_asökdhflakjslaslhfdlaffahflahsöfdhasägporejfäalkdsjäfalisjäfdlkasjd = 1
session = MagicMock()
models = MagicMock()

f()

f(1)

f(x=2)

f(1, x=2)

f(
    this_is_a_very_long_argument_asökdhflakjslaslhfdlaffahflahsöfdhasägporejfäalkdsjäfalisjäfdlkasjd
)
f(
    this_is_a_very_long_keyword_argument_asökdhflakjslaslhfdlaffahflahsöfdhasägporejfäalkdsjäfalisjäfdlkasjd=1
)

f(
    1,
    mixed_very_long_arguments=1,
)

f(
    this_is_a_very_long_argument_asökdhflakjslaslhfdlaffahflahsöfdhasägporejfäalkdsjäfalisjäfdlkasjd,
    these_arguments_have_values_that_need_to_break_because_they_are_too_long1=(100000 - 100000000000),
    these_arguments_have_values_that_need_to_break_because_they_are_too_long2="akshfdlakjsdfad" + "asdfasdfa",
    these_arguments_have_values_that_need_to_break_because_they_are_too_long3=session,
)

f(
    # dangling comment
)


f(
    only=1, short=1, arguments=1
)

f(
    hey_this_is_a_long_call, it_has_funny_attributes_that_breaks_into_three_lines=1
)

f(
    hey_this_is_a_very_long_call=1, it_has_funny_attributes_asdf_asdf=1, too_long_for_the_line=1, really=True
)

# TODO(konstin): Call chains/fluent interface (https://black.readthedocs.io/en/stable/the_black_code_style/current_style.html#call-chains)
result = (
    session.query(models.Customer.id)
    .filter(
        models.Customer.account_id == 10000,
        models.Customer.email == "user@example.org",
    )
    .order_by(models.Customer.id.asc())
    .all()
)
# TODO(konstin): Black has this special case for comment placement where everything stays in one line
f(
    "aaaaaaaa", "aaaaaaaa", "aaaaaaaa", "aaaaaaaa", "aaaaaaaa", "aaaaaaaa", "aaaaaaaa"
)

f(
    session,
    b=1,
    ** # oddly placed end-of-line comment
    dict()
)
f(
    session,
    b=1,
    **
    # oddly placed own line comment
    dict()
)
