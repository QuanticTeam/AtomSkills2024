import { useQuery, useQueryClient } from '@tanstack/react-query'
import {
  Button,
  Input,
  InputRef,
  Space,
  Spin,
  Table,
  TableColumnType,
  TableProps,
  Typography,
} from 'antd'
import { Ref, useEffect, useRef, useState } from 'react'
import { useTranslation } from 'react-i18next'
import { Link } from 'react-router-dom'
import { Lesson, LessonsApi } from '~/entities'
import { FilterType, SortAndFilterRequest, getSortAndFilterRequestPayload } from '~/shared/api'
import { ROUTE_PATH_LESSONS } from '~/shared/routing'
import { Oops } from '~/shared/ui'
import Highlighter, { HighlighterProps } from 'react-highlight-words'
import { SearchOutlined } from '@ant-design/icons'
import { FilterDropdownProps } from 'antd/es/table/interface'

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

  const handleReset = (clearFilters: () => void) => {
    clearFilters()
    setSearchText('')
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
            onClick={() => clearFilters && handleReset(clearFilters)}
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
    },
    {
      title: t('colTraits'),
      dataIndex: 'traits',
      render(value) {
        return JSON.stringify(value)
      },
    },
    {
      title: t('colSupplement'),
      dataIndex: 'supplements',
      key: 'supplements',
      render(value) {
        return JSON.stringify(value)
      },
    },
    {
      title: t('colTasks'),
      dataIndex: 'tasks',
      key: 'tasks',
      render(value) {
        return JSON.stringify(value)
      },
    },
    {
      title: t('colAuthor'),
      dataIndex: 'author',
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
      style={{ height: '100%' }}
      columns={columns}
      dataSource={data}
      pagination={false}
      scroll={{
        x: 0,
        y: 500,
      }}
      onChange={onTableChange}
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
