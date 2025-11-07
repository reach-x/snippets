{* Smarty template example - PHP templating engine *}

<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>{$pageTitle}</title>
</head>
<body>
    <header>
        <h1>{$title}</h1>
        <nav>
            <ul>
                {foreach $navItems as $item}
                <li><a href="{$item.url}">{$item.text}</a></li>
                {/foreach}
            </ul>
        </nav>
    </header>

    <main>
        <section>
            <h2>Welcome</h2>
            {if $user}
                <p>Hello, {$user.name}!</p>
            {else}
                <p>Please log in.</p>
            {/if}
        </section>

        <section>
            <h2>Users</h2>
            <ul>
                {foreach $users as $u}
                <li>
                    <strong>{$u.name}</strong> - {$u.email}
                    {if $u.active}
                        <span class="badge">Active</span>
                    {/if}
                </li>
                {foreachelse}
                <li>No users found.</li>
                {/foreach}
            </ul>
        </section>

        <section>
            <h2>Modifiers (Filters)</h2>
            <p>Uppercase: {$title|upper}</p>
            <p>Lowercase: {$title|lower}</p>
            <p>Capitalize: {$title|capitalize}</p>
            <p>Length: {$users|count}</p>
            <p>Default: {$missingVar|default:'N/A'}</p>
            <p>Date format: {$timestamp|date_format:'%Y-%m-%d %H:%M:%S'}</p>
            <p>Truncate: {$longText|truncate:50:'...'}</p>
            <p>Strip tags: {$htmlContent|strip_tags}</p>
            <p>Escape: {$userInput|escape:'html'}</p>
            <p>Replace: {$text|replace:'old':'new'}</p>
        </section>

        {* Variable assignment *}
        {assign var="message" value="Hello World"}
        <p>{$message}</p>

        {* Multiple assignments *}
        {assign var="x" value=5}
        {assign var="y" value=3}
        {assign var="sum" value=$x+$y}
        <p>Sum: {$sum}</p>

        {* Capture content *}
        {capture name="greeting"}
            Hello, {$user.name}! Welcome to {$siteName}.
        {/capture}
        <p>{$smarty.capture.greeting}</p>

        {* Array access *}
        <p>First user: {$users[0].name}</p>
        <p>User count: {$users|count}</p>

        {* Conditionals *}
        {if $age >= 18}
            <p>Adult</p>
        {elseif $age >= 13}
            <p>Teen</p>
        {else}
            <p>Child</p>
        {/if}

        {* Foreach with key *}
        {foreach $person as $key => $value}
            <p>{$key}: {$value}</p>
        {/foreach}

        {* Foreach with properties *}
        {foreach $items as $item}
            {$item@iteration}. {$item}
            {if $item@first}(first){/if}
            {if $item@last}(last){/if}
            Total: {$item@total}
        {/foreach}

        {* Section for complex iteration *}
        {section name=user loop=$users step=1}
            {$users[user].name}
        {/section}

        {* While loop *}
        {assign var="i" value=1}
        {while $i <= 5}
            <p>Number: {$i}</p>
            {assign var="i" value=$i+1}
        {/while}

        {* Include templates *}
        {include file="header.tpl"}
        {include file="user_card.tpl" user=$currentUser}

        {* Extends and blocks *}
        {extends file="layout.tpl"}
        {block name="content"}
            This is the content block
        {/block}

        {* Functions *}
        {function name="renderUser" user=null}
            <div class="user-card">
                <h3>{$user.name}</h3>
                <p>{$user.email}</p>
            </div>
        {/function}

        {call renderUser user=$currentUser}

        {* Math operations *}
        <p>Addition: {$x + $y}</p>
        <p>Multiplication: {$x * $y}</p>
        <p>Division: {10 / 2}</p>

        {* Smarty constants *}
        <p>Template: {$smarty.template}</p>
        <p>Current dir: {$smarty.current_dir}</p>
        <p>Version: {$smarty.version}</p>

        {* Config files *}
        {config_load file="site.conf"}
        <p>{#siteTitle#}</p>

        {* Literal blocks *}
        {literal}
            <script>
                var data = {name: "John"};
            </script>
        {/literal}

        {* Strip whitespace *}
        {strip}
            <ul>
                <li>Item 1</li>
                <li>Item 2</li>
            </ul>
        {/strip}

        {* Nocache blocks *}
        {nocache}
            Current time: {$smarty.now|date_format:'%H:%M:%S'}
        {/nocache}
    </main>

    <footer>
        <p>&copy; {$smarty.now|date_format:'%Y'} {$siteName}</p>
    </footer>
</body>
</html>
