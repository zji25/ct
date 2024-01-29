### лабораторная работа #3. использование автоматических генераторов анализаторов Bison и ANTLR

#### вариант 9. подсветка синтаксиса в html
выберите подмножества языка c++, java или python и напишите конвертор программ на этом языке в html с подсветкой синтаксиса

пример:
```
int main() {
    int a, b;
    scanf("%d%d", &a, &b);
    printf("%d\n", a + b);
    return 0;
}
```

вывод (сделайте лучше, чем тут):
```
<b>int</b> main() {<br>
&nbsp;&nbsp;&nbsp;&nbsp;<b>int</b> a, b;<br>
&nbsp;&nbsp;&nbsp;&nbsp;scanf("%d%d", &a, &b);<br>
&nbsp;&nbsp;&nbsp;&nbsp;printf("%d\n", a + b);<br>
&nbsp;&nbsp;&nbsp;&nbsp;<b>return</b> 0;<br>
}
```

#### модификация
добавить ```while```