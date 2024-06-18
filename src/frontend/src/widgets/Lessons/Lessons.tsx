import { useQuery, useQueryClient } from '@tanstack/react-query'
import {
  Button,
  Input,
  InputRef,
  Popover,
  Space,
  Spin,
  Table,
  TableColumnType,
  TableProps,
  Tag,
  Tooltip,
  Typography,
} from 'antd'
import { Ref, useEffect, useRef, useState } from 'react'
import { useTranslation } from 'react-i18next'
import { Link } from 'react-router-dom'
import { Lesson, LessonsApi } from '~/entities'
import { FilterType, SortAndFilterRequest, getSortAndFilterRequestPayload } from '~/shared/api'
import { ROUTE_PATH_LESSONS } from '~/shared/routing'
import { Markdown, Oops } from '~/shared/ui'
import Highlighter, { HighlighterProps } from 'react-highlight-words'
import { SearchOutlined } from '@ant-design/icons'
import { FilterDropdownProps } from 'antd/es/table/interface'
import { Tasks } from '../Tasks'

interface LessonsProps {
  parentRef?: Ref<HTMLDivElement>
}

type DataIndex = keyof Lesson

export function Lessons({ parentRef }: LessonsProps) {
  const { t } = useTranslation(Lessons.name)

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

  const getColumnSearchProps = (dataIndex: DataIndex): TableColumnType<Lesson> => ({
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
            Search
          </Button>
          <Button
            onClick={() => clearFilters && handleReset(clearFilters, confirm)}
            size="small"
            style={{ width: 90 }}
          >
            Reset
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

  const columns: TableProps<Lesson>['columns'] = [
    {
      title: t('colCode'),
      dataIndex: 'code',
      key: 'code',
      ...getColumnSearchProps('code'),
    },
    {
      title: t('colTitle'),
      dataIndex: 'title',
      sorter: true,
      key: 'title',
      ...getColumnSearchProps('title'),
      render(value, record) {
        return (
          <Link to={`${ROUTE_PATH_LESSONS}/${record.code}`}>
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
    {
      title: t('colTraits'),
      dataIndex: 'traits',
      render(value) {
        return value.map((x: any) => (
          <Tag
            key={x.code}
            color="orange-inverse"
          >
            {x.name}
          </Tag>
        ))
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
      title: t('colAuthor'),
      dataIndex: 'author',
      sorter: true,
      key: 'author',
      ...getColumnSearchProps('title'),
    },
  ]

  const [sortAndFilterPayload, setSortAndFilterPayload] = useState<SortAndFilterRequest>(
    getSortAndFilterRequestPayload(),
  )

  const { data, error, isPending, refetch } = useQuery({
    queryKey: ['lessons'],
    queryFn: () => LessonsApi.getAll(sortAndFilterPayload),
  })

  useEffect(() => {
    refetch()
  }, [refetch, sortAndFilterPayload])

  const onTableChange: TableProps<Lesson>['onChange'] = (pagination, filters, sorter, extra) => {
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

  return isPending ? (
    <Spin />
  ) : error ? (
    <Oops />
  ) : (
    <Table
      rowKey="code"
      bordered
      style={{ height: '100%' }}
      columns={columns}
      dataSource={data}
      pagination={false}
      scroll={{
        x: 0,
        y: 500,
      }}
      onChange={onTableChange}
      expandable={{
        expandedRowRender: lesson => (
          <>
            <Typography.Text strong>Задания</Typography.Text>
            <Tasks
              lessonCode={lesson.code}
              tasks={lesson.tasks as any}
            />
          </>
        ),
      }}
    />
  )
}

function SearchHighlighter({ searchWords, text }: HighlighterProps) {
  return (
    <Highlighter
      highlightStyle={{ backgroundColor: '#ffc069', padding: 0 }}
      searchWords={searchWords}
      autoEscape
      textToHighlight={text ? text.toString() : ''}
    />
  )
}
