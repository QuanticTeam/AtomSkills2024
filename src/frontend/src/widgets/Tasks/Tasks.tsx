import { SearchOutlined } from '@ant-design/icons'
import { useQuery } from '@tanstack/react-query'
import {
  Button,
  Input,
  InputRef,
  Popover,
  Space,
  Table,
  TableColumnType,
  TableProps,
  Typography,
} from 'antd'
import { FilterDropdownProps } from 'antd/es/table/interface'
import { useState, useEffect, useRef } from 'react'
import { useTranslation } from 'react-i18next'
import { Link } from 'react-router-dom'
import { Lesson, LessonsApi, Task, TasksApi } from '~/entities'
import { SortAndFilterRequest, getSortAndFilterRequestPayload, FilterType } from '~/shared/api'
import { ROUTE_PATH_LESSONS, ROUTE_PATH_LOREM } from '~/shared/routing'
import { Markdown } from '~/shared/ui'

export interface TasksProps {
  tasks: Task[]
  lessonCode: Lesson['code']
}

type DataIndex = keyof Task

export function Tasks({ tasks, lessonCode }: TasksProps) {
  const { t } = useTranslation(Tasks.name)

  const [searchText, setSearchText] = useState('')
  const [searchedColumn, setSearchedColumn] = useState('')
  const searchInput = useRef<InputRef>(null)

  const handleSearch = (
    selectedKeys: string[],
    confirm: FilterDropdownProps['confirm'],
    dataIndex: DataIndex,
  ) => {
    confirm()
    setSearchText(selectedKeys[0])
    setSearchedColumn(dataIndex)
  }

  const handleReset = (clearFilters: () => void, confirm: () => void) => {
    clearFilters()
    setSearchText('')
    setSearchedColumn('')
    confirm()
  }

  const getColumnSearchProps = (dataIndex: DataIndex): TableColumnType<Task> => ({
    filterDropdown: ({ setSelectedKeys, selectedKeys, confirm, clearFilters, close }) => (
      <div
        style={{ padding: 8 }}
        onKeyDown={e => e.stopPropagation()}
      >
        <Input
          ref={searchInput}
          placeholder={`Search ${dataIndex}`}
          value={selectedKeys[0]}
          onChange={e => setSelectedKeys(e.target.value ? [e.target.value] : [])}
          onPressEnter={() => handleSearch(selectedKeys as string[], confirm, dataIndex)}
          style={{ marginBottom: 8, display: 'block' }}
        />
        <Space>
          <Button
            type="primary"
            onClick={() => handleSearch(selectedKeys as string[], confirm, dataIndex)}
            icon={<SearchOutlined />}
            size="small"
            style={{ width: 90 }}
          >
            Поиск
          </Button>
          <Button
            onClick={() => clearFilters && handleReset(clearFilters, confirm)}
            size="small"
            style={{ width: 90 }}
          >
            Сбросить
          </Button>
        </Space>
      </div>
    ),
    filterIcon: (filtered: boolean) => (
      <SearchOutlined style={{ color: filtered ? '#1677ff' : undefined }} />
    ),
    // onFilter: (value, record) =>
    //   record[dataIndex]
    //     .toString()
    //     .toLowerCase()
    //     .includes((value as string).toLowerCase()),
    onFilterDropdownOpenChange: visible => {
      if (visible) {
        setTimeout(() => searchInput.current?.select(), 100)
      }
    },
    // render: text =>
    //   searchedColumn === dataIndex ? (
    //     <SearchHighlighter
    //       searchWords={[searchText]}
    //       textToHighlight={text}
    //     />
    //   ) : (
    //     text
    //   ),
  })

  const columns: TableProps<Task>['columns'] = [
    {
      title: t('colCode'),
      dataIndex: 'code',
      key: 'code',
      ...getColumnSearchProps('code'),
    },
    {
      title: t('colTitle'),
      dataIndex: 'title',
      key: 'title',
      ...getColumnSearchProps('title'),
      render(value, record) {
        return (
          <Link to={`${ROUTE_PATH_LESSONS}/${lessonCode}/tasks/${record.code}`}>
            <Typography.Link>{value}</Typography.Link>
          </Link>
        )
      },
    },
    {
      title: t('colContent'),
      dataIndex: 'content',
      key: 'content',
      ...getColumnSearchProps('content'),
      render(value) {
        return (
          <Popover
            trigger="click"
            content={<Markdown markdown={value} />}
            placement="right"
            overlayInnerStyle={{
              width: '40vw',
              height: '40vh',
              overflow: 'auto',
            }}
          >
            <Typography.Link className="border-b  border-dashed border-blue-600">
              {value.split(' ').slice(0, 10).join(' ') + '...'}
            </Typography.Link>
          </Popover>
        )
      },
    },
    // {
    //   title: t('colSupplement'),
    //   dataIndex: 'supplements',
    //   key: 'supplements',
    //   render(value) {
    //     return JSON.stringify(value)
    //   },
    // },
    {
      title: t('colDifficulty'),
      dataIndex: 'difficulty',
      ...getColumnSearchProps('difficulty'),
      key: 'difficulty',
    },
    {
      title: t('colTime'),
      dataIndex: 'time',
      key: 'time',
      ...getColumnSearchProps('time'),
      render(value) {
        return JSON.stringify(value)
      },
    },
    {
      title: 'Рейтинг',
      key: 'rank',
      render(_, record) {
        return (
          <Typography.Link>
            <Link to={ROUTE_PATH_LOREM + '/task/' + record.code}>Перейти</Link>
          </Typography.Link>
        )
      },
    },
  ]

  const [sortAndFilterPayload, setSortAndFilterPayload] = useState<SortAndFilterRequest>(
    getSortAndFilterRequestPayload(),
  )

  const { data, error, isPending, refetch } = useQuery({
    initialData: tasks,
    queryKey: ['tasks'],
    queryFn: () => TasksApi.getAll({ ...sortAndFilterPayload, omCode: lessonCode }),
  })

  useEffect(() => {
    refetch()
  }, [refetch, sortAndFilterPayload])

  const onTableChange: TableProps<Task>['onChange'] = (pagination, filters, sorter, extra) => {
    switch (extra.action) {
      case 'sort': {
        if (!Array.isArray(sorter)) {
          console.log(sorter)
          setSortAndFilterPayload(prev => ({
            ...prev,
            orderBy: sorter.order ? (sorter.columnKey as string) : '',
            descending: sorter.order === ('descend' as string),
          }))
        }

        break
      }

      case 'filter': {
        setSortAndFilterPayload(prev => ({
          ...prev,
          filters: Object.entries(filters)
            .filter(x => x[1] && x[1].length)
            .map(([columnName, values]) => ({
              columnName,
              values: (values as string[]) ?? [],
              filterType: FilterType.Contains,
            })),
        }))

        break
      }
    }

    console.log('params', { pagination, filters, sorter, extra })
  }

  return (
    <Table
      bordered
      columns={columns}
      dataSource={data}
      pagination={false}
      onChange={onTableChange}
      scroll={{
        x: 0,
        y: 600,
      }}
    />
  )
}
